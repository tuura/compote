/*
	Author: Andrey Mokhov, Newcastle University
	Date: 12 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: BDD base implementation.
*/

#include <algorithm>
#include <limits>
#include "base.h"

namespace bdd
{
	Base::Base(size_t cacheSize)
		: sink(getNodeID(nullptr), getNodeID(nullptr), Node::sinkVariable, 1)
		, one(getNodeID(&sink))
		, zero(inverted(one))
		, cache_f(cacheSize, zero)
		, cache_g(cacheSize, zero)
		, cache_h(cacheSize, zero)
		, cache_r(cacheSize, zero)
		, deadCount(0)
		{
			assert((cacheSize & (cacheSize - 1)) == 0);
		}

	void Base::setCacheSize(size_t cacheSize)
	{
		assert((cacheSize & (cacheSize - 1)) == 0);
		cache_f.assign(cacheSize, zero);
		cache_g.assign(cacheSize, zero);
		cache_h.assign(cacheSize, zero);
		cache_r.assign(cacheSize, zero);
	}

	bool Base::canonise(NodeID &f, NodeID &g, NodeID &h) const
	{
		// Detecting implicit constants:
		// C1. (f, f, g) -> (f, 1, g)
		if (f == g) g = one;

		// C2. (f, g, f) -> (f, g, 0)
		if (f == h) h = zero;

		// C3. (f, ¬f, g) -> (f, 0, g)
		if (g == inverted(f)) g = zero;

		// C4. (f, g, ¬f) -> (f, g, 1)
		if (h == inverted(f)) h = one;

		// Finding canonical order:
		// O1. ite(f, 1, h) = ite( h, 1,  f)
		// O2. ite(f, 0, h) = ite(¬h, 0, ¬f)
		if (cmpNodeID(h, f))
		{
			if (g == one)
			{
				std::swap(f, h);
			}
			if (g == zero)
			{
				std::swap(f, h);
				invert(f);
				invert(h);
			}
		}

		// O3. ite(f, g,  1) = ite(¬g, ¬f,  1)
		// O4. ite(f, g,  0) = ite( g,  f,  0)
		// O5. ite(f, g, ¬g) = ite( g,  f, ¬f)
		if (cmpNodeID(g, f))
		{
			if (h == one)
			{
				std::swap(f, g);
				invert(f);
				invert(g);
			}
			if (h == zero)
			{
				std::swap(f, g);
			}
			if (h == inverted(g))
			{
				h = inverted(f);
				std::swap(f, g);
			}
		}

		// Finding canonical polarity by ensuring positive f and g:
		// P1. ite(¬f, g, h) =  ite(f, h, g)
		if (isNegative(f))
		{
			invert(f);
			std::swap(g, h);
		}

		// P2. ite(f, ¬g, h) = ¬ite(f, g, ¬h)
		if (isNegative(g))
		{
			invert(g);
			invert(h);
			return true;
		}
		return false;
	}

	inline size_t hashIte(NodeID f, NodeID g, NodeID h) { return hashNodeID(f) ^ hashNodeID(g) ^ hashNodeID(h); }

	NodeID Base::fetchNode(int v, NodeID low, NodeID high)
	{
		// Check if the hash table contains node (v, low, high)
		Node node(low, high, v, 0), *res = nullptr;

		const auto it = table.find(&node);

		if (it == table.end())
		{
			res = new Node(node);
			// printNodeID(getNodeID(res), "New node created: ");
			table.insert(res);
			deadCount++; // Node is initially dead (it will soon be revived by referencing).
		}
		else
		{
			res = *it;
		}

		assert(res);

		return getNodeID(res);
	}

	NodeID Base::ite(NodeID f, NodeID g, NodeID h)
	{
		const bool invertResult = canonise(f, g, h);
		assert(isPositive(f) && isPositive(g));

		// Testing terminal cases: ite(1, g, _) = ite(_, g, g) = g
		if (f == one || g == h)
		{
			return referenceNodeID(invertedIf(g, invertResult));
		}

		// Check if ite(f, g, h) is in cache.
		const size_t hsh = hashIte(f, g, h) & (cache_f.size() - 1);
		if (cache_f[hsh] == f && cache_g[hsh] == g && cache_h[hsh] == h)
		{
			return referenceNodeID(invertedIf(cache_r[hsh], invertResult));
		}

		const Node *pf = getNodePtr(f); assert(pf);
		const Node *pg = getNodePtr(g); assert(pg);
		const Node *ph = getNodePtr(h); assert(ph);

		const int v = std::min(pf->var, std::min(pg->var, ph->var));
		assert(v != Node::sinkVariable);

		const NodeID low  = dereferenceNodeID(ite(pf->var == v ? pf->low : f,
										  	  	  pg->var == v ? pg->low : g,
										  		  ph->var == v ? invertedIf(ph->low, isNegative(h)) : h));

		const NodeID high = dereferenceNodeID(ite(pf->var == v ? pf->high : f,
										  		  pg->var == v ? pg->high : g,
										  		  ph->var == v ? invertedIf(ph->high, isNegative(h)) : h));

		assert(isPositive(low));

		if (low == high)
		{
			remember(hsh, f, g, h, low);
			return referenceNodeID(invertedIf(low, invertResult));
		}

		const NodeID resID = fetchNode(v, low, high);
		remember(hsh, f, g, h, resID);
		return referenceNodeID(invertedIf(resID, invertResult));
	}

	NodeID Base::iteConst(NodeID f, NodeID g, NodeID h)
	{
		const bool invertResult = canonise(f, g, h);
		assert(isPositive(f) && isPositive(g));

		// Testing terminal cases: ite(1, g, _) = ite(_, g, g) = g
		if (f == one || g == h)
		{
			return invertedIf(g, invertResult);
		}

		// Check if ite(f, g, h) is in cache.
		const size_t hsh = hashIte(f, g, h) & (cache_f.size() - 1);
		if (cache_f[hsh] == f && cache_g[hsh] == g && cache_h[hsh] == h)
		{
			return invertedIf(cache_r[hsh], invertResult);
		}

		const Node *pf = getNodePtr(f); assert(pf);
		const Node *pg = getNodePtr(g); assert(pg);
		const Node *ph = getNodePtr(h); assert(ph);

		const int v = std::min(pf->var, std::min(pg->var, ph->var));
		assert(v != Node::sinkVariable);

		const NodeID low = iteConst(pf->var == v ? pf->low : f,
							   		pg->var == v ? pg->low : g,
						  	   		ph->var == v ? invertedIf(ph->low, isNegative(h)) : h);

		if (low == nonConst || low != one && low != zero) return nonConst;

		const NodeID high = iteConst(pf->var == v ? pf->high : f,
						  	   		 pg->var == v ? pg->high : g,
						  	   		 ph->var == v ? invertedIf(ph->high, isNegative(h)) : h);

		assert(isPositive(low));

		if (high == nonConst || low != high) return nonConst;

		remember(hsh, f, g, h, low);

		return invertedIf(low, invertResult);
	}

	void Base::referenceNode(Node *node)
	{
		// printNodeID(getNodeID(node), "Referencing ");
		assert(node);
		if (node == &sink) return;

		assert(node->refs != std::numeric_limits<unsigned>::max());
		node->refs++;
		if (node->refs == 1)
		{
			deadCount--;
			referenceNode(getNodePtr(node->low ));
			referenceNode(getNodePtr(node->high));
		}
	}

	void Base::dereferenceNode(Node *node)
	{
		// printNodeID(getNodeID(node), "Dereferencing ");
		assert(node);
		if (node == &sink) return;

		assert(node->refs);
		if (node->refs == 1)
		{
			dereferenceNode(getNodePtr(node->low ));
			dereferenceNode(getNodePtr(node->high));
			deadCount++;
		}
		node->refs--;
		if (deadCount * 2 > table.size()) runGC();
	}

	void Base::runGC()
	{
		// Cleanup cache.
		for(size_t i = 0; i < cache_r.size(); i++)
		if (!getNodePtr(cache_r[i])->refs)
		{
			cache_f[i] = cache_g[i] = cache_h[i] = cache_r[i] = zero;
		}

		// Cleanup hash table and delete dead nodes.
		for(auto it = table.begin(); it != table.end(); )
		{
			Node *node = *it;
			assert(node);
			if (node->refs)
			{
				++it;
				continue;
			}

			// printNodeID(getNodeID(node), "Node deleted: ");
			delete node;
			it = table.erase(it);
			deadCount--;
		}

		assert(!deadCount);
	}

	void Base::clear()
	{
		// Cleanup cache.
		setCacheSize(cache_f.size());

		// Cleanup hash table.
		for(auto it = table.begin(); it != table.end(); )
		{
			Node *node = *it;
			// printNodeID(getNodeID(node), "Node deleted: ");
			delete node;
			it = table.erase(it);
		}

		deadCount = 0;
	}
}

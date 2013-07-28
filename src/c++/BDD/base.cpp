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
		: sink(getNodeID(nullptr), invert(getNodeID(nullptr)), Node::sinkVariable, 1) // invert to maintain low != high invariant
		, one(getNodeID(&sink))
		, zero(invert(one))
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

	// A total order on BDD nodes.
	// Sink nodes precede others.
	bool operator<(NodeID fid, NodeID gid)
	{
		Node *f = getNodePtr(fid);
		Node *g = getNodePtr(gid);
		assert(f && g);

		if (f->var != g->var)
		{
			if (f->var == Node::sinkVariable) return true;
			if (g->var == Node::sinkVariable) return false;
			return f->var < g->var;
		}

		return f < g;
	}

	// Converts ite(f, g, h) into canonical form.
	// Returns true if the result must be inverted.
	bool Base::canonise(NodeID &f, NodeID &g, NodeID &h) const
	{
		// Detecting implicit constants:
		// C1. (f, f, g) -> (f, 1, g)
		if (f == g) g = one;

		// C2. (f, g, f) -> (f, g, 0)
		if (f == h) h = zero;

		// C3. (f, ¬f, g) -> (f, 0, g)
		if (g == invert(f)) g = zero;

		// C4. (f, g, ¬f) -> (f, g, 1)
		if (h == invert(f)) h = one;

		// Finding canonical order:
		// O1. ite(f, 1, h) = ite( h, 1,  f)
		// O2. ite(f, 0, h) = ite(¬h, 0, ¬f)
		if (h < f)
		{
			if (g == one)
			{
				std::swap(f, h);
			}
			else
			if (g == zero)
			{
				std::swap(f, h);
				f = invert(f);
				h = invert(h);
			}
		}

		// O3. ite(f, g,  1) = ite(¬g, ¬f,  1)
		// O4. ite(f, g,  0) = ite( g,  f,  0)
		// O5. ite(f, g, ¬g) = ite( g,  f, ¬f)
		if (g < f)
		{
			if (h == one)
			{
				std::swap(f, g);
				f = invert(f);
				g = invert(g);
			}
			else
			if (h == zero)
			{
				std::swap(f, g);
			}
			if (h == invert(g))
			{
				h = invert(f);
				std::swap(f, g);
			}
		}

		// Finding canonical polarity by ensuring positive f and g:
		// P1. ite(¬f, g, h) =  ite(f, h, g)
		if (isNegative(f))
		{
			f = invert(f);
			std::swap(g, h);
		}

		// P2. ite(f, ¬g, h) = ¬ite(f, g, ¬h)
		if (isNegative(g))
		{
			g = invert(g);
			h = invert(h);
			return true;
		}
		return false;
	}

	inline size_t hashIte(NodeID f, NodeID g, NodeID h) { return hashNodeID(f) ^ hashNodeID(g) ^ hashNodeID(h); }

	// Fetch the node from the hash table, or create a new one.
	// Reference counter of the returned node is incremented.
	NodeID Base::fetchNode(int v, NodeID low, NodeID high)
	{
		assert(low != high);

		// Check if the hash table contains node (v, low, high)
		Node node(low, high, v, 1), *res = nullptr;

		const auto it = table.find(&node);

		if (it == table.end())
		{
			// TODO: Not thread-safe (this node could have already been created in another thread).
			res = new Node(node);
			// printNodeID(getNodeID(res), "New node created: ");
			table.insert(res);
			referenceNodeID(res->low);
			referenceNodeID(res->high);
		}
		else
		{
			res = *it;
			// TODO: Not thread-safe (if a dead node is found it can be removed by GC before the increment below revives it).
			referenceNode(res);
		}

		assert(res);
		assert(res->refs);

		return getNodeID(res);
	}

	// Construct BDD for function ite(f, g, h) = f g + ¬f h.
	// Reference counter of the returned node is incremented.
	NodeID Base::ite(NodeID f, NodeID g, NodeID h)
	{
		const bool invertResult = canonise(f, g, h);
		assert(isPositive(f) && isPositive(g));

		// Testing terminal cases: ite(1, g, _) = ite(_, g, g) = g
		if (f == one || g == h)
		{
			return invertIf(referenceNodeID(g), invertResult);
		}

		// Check if ite(f, g, h) is in cache.
		const size_t hsh = hashIte(f, g, h) & (cache_f.size() - 1);
		if (cache_f[hsh] == f && cache_g[hsh] == g && cache_h[hsh] == h)
		{
			// TODO: Not thread-safe (cache_r[hsh] might have been rewritten in other thread).
			return invertIf(referenceNodeID(cache_r[hsh]), invertResult);
		}

		const Node *pf = getNodePtr(f); assert(pf);
		const Node *pg = getNodePtr(g); assert(pg);
		const Node *ph = getNodePtr(h); assert(ph);

		const int v = std::min(pf->var, std::min(pg->var, ph->var));
		assert(v != Node::sinkVariable);

		const NodeID low  = ite(pf->var == v ? pf->low : f,
								pg->var == v ? pg->low : g,
								ph->var == v ? invertIf(ph->low, isNegative(h)) : h);

		const NodeID high = ite(pf->var == v ? pf->high : f,
								pg->var == v ? pg->high : g,
								ph->var == v ? invertIf(ph->high, isNegative(h)) : h);

		assert(isPositive(low));
		dereferenceNodeID(high);

		if (low == high)
		{
			remember(hsh, f, g, h, low);
			return invertIf(low, invertResult);
		}

		dereferenceNodeID(low);
		const NodeID resID = fetchNode(v, low, high);
		remember(hsh, f, g, h, resID);
		return invertIf(resID, invertResult);
	}

	// Check if function ite(f, g, h) = f g + ¬f h is constant.
	// Returns 0, 1, or a non-constant node ID, not necessarily equal to ite(f, g, h).
	// No intermediate BDD nodes are created in the process.
	// Reference counter of the returned node is NOT incremented.
	NodeID Base::iteConst(NodeID f, NodeID g, NodeID h)
	{
		const bool invertResult = canonise(f, g, h);
		assert(isPositive(f) && isPositive(g));

		// Testing terminal cases: ite(1, g, _) = ite(_, g, g) = g
		if (f == one || g == h)
		{
			return invertIf(g, invertResult);
		}

		// Check if ite(f, g, h) is in cache.
		const size_t hsh = hashIte(f, g, h) & (cache_f.size() - 1);
		if (cache_f[hsh] == f && cache_g[hsh] == g && cache_h[hsh] == h)
		{
			// TODO: Not thread-safe (cache_r[hsh] might have been rewritten in other thread).
			return invertIf(cache_r[hsh], invertResult);
		}

		const Node *pf = getNodePtr(f); assert(pf);
		const Node *pg = getNodePtr(g); assert(pg);
		const Node *ph = getNodePtr(h); assert(ph);

		const int v = std::min(pf->var, std::min(pg->var, ph->var));
		assert(v != Node::sinkVariable);

		const NodeID low = iteConst(pf->var == v ? pf->low : f,
									pg->var == v ? pg->low : g,
									ph->var == v ? invertIf(ph->low, isNegative(h)) : h);

		if (low == nonConst || low != one && low != zero) return nonConst;

		const NodeID high = iteConst(pf->var == v ? pf->high : f,
									 pg->var == v ? pg->high : g,
									 ph->var == v ? invertIf(ph->high, isNegative(h)) : h);

		assert(isPositive(low));

		if (high == nonConst || low != high) return nonConst;

		remember(hsh, f, g, h, low);

		return invertIf(low, invertResult);
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
			assert(node->low != node->high);
			referenceNodeID(node->low );
			referenceNodeID(node->high);
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
			assert(node->low != node->high);
			dereferenceNodeID(node->low );
			dereferenceNodeID(node->high);
			deadCount++;
		}
		node->refs--;
		if (deadCount * 2 > table.size() + 1000) runGC();
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
			if (!node->refs) deadCount--;
			delete node;
			it = table.erase(it);
		}

		assert(!deadCount);
	}
}

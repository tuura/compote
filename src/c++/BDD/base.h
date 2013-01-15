/*
	Author: Andrey Mokhov, Newcastle University
	Date: 12 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: BDD base implementation.
*/

#pragma once
#include <unordered_set>
#include "node.h"

namespace bdd
{
	class Base final
	{
		// The only sink node in the BDD, representing constant 1 function.
		const Node sink;

		public:

		// IDs of nodes, representing constant 1 and 0 functions.
		const NodeID one;
		const NodeID zero;

		// ID representing a non-constant node (used in iteConst).
		static const NodeID nonConst = NodeID::nullNode;

		private:

		// Cache table for keeping track of recent BDD computations.
		std::vector<NodeID> cache_f, cache_g, cache_h, cache_r;
		void remember(size_t hsh, NodeID f, NodeID g, NodeID h, NodeID res)
		{
			cache_f[hsh] = f;
			cache_g[hsh] = g;
			cache_h[hsh] = h;
			cache_r[hsh] = res;
		}

		public:

		// Default cache table size is 2^10 = 1024. Call setCacheSize to change it.
		static const size_t defaultCacheSize = 1 << 10;

		// Change size of the cache table (the table is cleared in the process).
		// Note: cacheSize must be a power of 2.
		void setCacheSize(size_t cacheSize);

		private:
		// Hash table of all BDD nodes.
		std::unordered_set<Node*, HashNodePtr, EqNodePtr> table;

		// The current number of dead nodes (i.e. whose refs == 0) in the table.
		size_t deadCount;

		// Fetch the node from the hash table, or create a new one.
		// Reference counter of the returned node is incremented.
		NodeID fetchNode(int v, NodeID low, NodeID high);

		// Converts ite(f, g, h) into canonical form.
		// Returns true if the result must be inverted.
		bool canonise(NodeID &f, NodeID &g, NodeID &h) const;

		public:

		// Construct a BDD with required cache size (defaulted to defaultCacheSize).
		// Note: cacheSize must be a power of 2.
		Base(size_t cacheSize = defaultCacheSize);

		// Construct BDD for function ite(f, g, h) = f g + ¬f h.
		// Reference counter of the returned node is incremented.
		NodeID ite(NodeID f, NodeID g, NodeID h);

		// Check if function ite(f, g, h) = f g + ¬f h is constant.
		// Returns 0, 1, or a non-constant node ID, not necessarily equal to ite(f, g, h).
		// No intermediate BDD nodes are created in the process.
		// Reference counter of the returned node is NOT incremented.
		NodeID iteConst(NodeID f, NodeID g, NodeID h);

		// Construct BDDs for basic logic functions.
		// Reference counter of the returned node is incremented.
		NodeID andGate(NodeID f, NodeID g) { return ite(f, g, zero); }
		NodeID  orGate(NodeID f, NodeID g) { return ite(f, one, g); }
		NodeID xorGate(NodeID f, NodeID g) { return ite(f, invert(g), g); }
		NodeID notGate(NodeID f          ) { return referenceNodeID(invert(f)); }

		// Check if f => g is a tautology.
		bool imply(NodeID f, NodeID g) { return iteConst(f, g, one) == one; }

		// Construct a variable node, possible negated.
		// Reference counter of the returned node is incremented.
		NodeID variable(int var, bool positive = true)
		{
			return invertIf(fetchNode(var, one, zero), positive);
		}

		// Increase reference count of a node (if initially 0, apply recursively to children).
		NodeID referenceNodeID(NodeID id) { referenceNode(getNodePtr(id)); return id; }
		void referenceNode(Node *node);

		// Dereference a node (if reference counter reaches 0, apply recursively to children).
		// Dereferenced nodes will be deleted eventually unless they are referenced again.
		// Triggers garbage collection if (deadCount * 2 > table.size()).
		NodeID dereferenceNodeID(NodeID id) { dereferenceNode(getNodePtr(id)); return id; }
		void dereferenceNode(Node *node);

		// Garbage collection: remove all dead nodes from the cache and hash tables.
		void runGC();

		// Remove all bdd nodes bringing the object to its initial state.
		// Called from the desctructor.
		void clear();

		// Statistics.
		size_t size() const { return table.size(); }
		size_t sizeDead() const { return deadCount; }

		// Destructor.
		// Note that it is non-virtual, as the whole class is declared final.
		~Base()
		{
			clear();
		}
	};
}

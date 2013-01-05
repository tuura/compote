/*
	Author: Andrey Mokhov, Newcastle University
	Date: 13 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: BDD nodes and related functionality.
*/

#pragma once
#include <algorithm>
#include <cassert>

namespace bdd
{
	// BDD node ID supporting inversion by flipping the lowest bit.
	// Node nullNode corresponds to NULL pointer.
	enum class NodeID : intptr_t { nullNode = (intptr_t)nullptr };

	// BDD node ID inversion bit manipulations.
	inline bool   isPositive(NodeID id)         { return ((intptr_t)id & 1) == 0;    }
	inline bool   isNegative(NodeID id)         { return ((intptr_t)id & 1) == 1;    }
	inline NodeID inverted  (NodeID id)         { return (NodeID)((intptr_t)id ^ 1); }
	inline NodeID invertedIf(NodeID id, bool c) { return (NodeID)((intptr_t)id ^ c); }

	inline void invert(NodeID &id) { id = inverted(id); }

	// BDD node with reference counter.
	struct Node
	{
		NodeID low;
		NodeID high;

		int var;
		unsigned refs;

		static const int sinkVariable = std::numeric_limits<int>::max();

		Node(NodeID low, NodeID high, int var,	unsigned refs) : low(low), high(high), var(var), refs(refs)
		{
			assert(isPositive(low));
		}
		Node(const Node &node) : low(node.low), high(node.high), var(node.var), refs(node.refs)
		{
			assert(isPositive(low));
		}
	};

	// Conversion between BDD node IDs and BDD node pointers.
	inline NodeID getNodeID (const Node *node) { return (NodeID)(intptr_t)node; }
	inline Node  *getNodePtr(NodeID        id) { return (Node*)((intptr_t)id & ~(intptr_t)1); }

	// Hash functions.
	inline size_t hashNodeID(NodeID id)
	{
		return std::hash<Node*>()(getNodePtr(id));
	}

	inline size_t hashNodePtr(const Node *node)
	{
		return hashNodeID(node->low) ^ hashNodeID(node->high) ^ std::hash<int>()(node->var);
	}

	// A total order on BDD nodes.
	inline bool cmpNodeID(NodeID f, NodeID g)
	{
		auto pf = getNodePtr(f);
		auto pg = getNodePtr(g);
		return (pf->var < pg->var) || (pf->var == pg->var && f < g);
	}

	// Comparing two nodes for equality.
	inline bool eqNodePtr(const Node *p, const Node *q)
	{
		return p->low == q->low && p->high == q->high && p->var == q->var;
	}

	// Auxilliary function objects.
	struct HashNodePtr
	{
		size_t operator()(Node* const &ptr) const { return hashNodePtr(ptr); }
	};

	struct EqNodePtr
	{
		bool operator()(Node* const &p, Node* const &q) const { return eqNodePtr(p, q); }
	};

	// Printing out debug information on a node.
	inline void printNodeID(NodeID node, char *comment = nullptr)
	{
		if (comment) printf("%s", comment);

		printf("[%p]: ", node);

		Node *ptr = getNodePtr(node);
		assert(ptr);

		if (ptr->var == Node::sinkVariable)
		{
			printf("%u\n", isPositive(node));
			return;
		}
		else
		{
			if (isNegative(node)) printf("not ");
			printf("(%u, ", ptr->var);
		}

		assert(getNodePtr(ptr->low) && getNodePtr(ptr->high));
		assert(isPositive(ptr->low));

		if (getNodePtr(ptr->low)->var != Node::sinkVariable)
			printf("%p, ", ptr->low);
		else
			printf("%u, ", isPositive(ptr->low ));

		if (getNodePtr(ptr->high)->var != Node::sinkVariable)
			printf("%p)\n", ptr->high);
		else 
			printf("%u)\n", isPositive(ptr->high));
	}
}

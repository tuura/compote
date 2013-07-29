/*
	Author: Andrey Mokhov, Newcastle University
	Date: 21 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Instance of BDD Base class accessible from Haskell through FFI.
*/

#include <cassert>
#include "base.h"

using namespace bdd;
Base base;

extern "C"
{
	NodeID one () { return base.referenceNodeID(base.one);  }
	NodeID zero() { return base.referenceNodeID(base.zero); }

	NodeID variable(int v) { return base.variable(v); }

	NodeID ite(NodeID f, NodeID g, NodeID h) { return base.ite(f, g, h); }

	NodeID andGate(NodeID f, NodeID g) { return base.andGate(f, g); }
	NodeID  orGate(NodeID f, NodeID g) { return base. orGate(f, g); }
	NodeID xorGate(NodeID f, NodeID g) { return base.xorGate(f, g); }
	NodeID notGate(NodeID f          ) { return base.notGate(f   ); }

	int iteTrue(NodeID f, NodeID g, NodeID h) { return (int)(base.iteConst(f, g, h) == base.one); }

	int imply(NodeID f, NodeID g) { return (int)base.imply(f, g); }

	void dereference(NodeID node) { base.dereferenceNodeID(node); }

	void setCacheSize(size_t cacheSize) { base.setCacheSize(cacheSize); }

	// TODO: think if it make sense to make these visible in Haskell
	// void runGC() { base.runGC(); }
	// void clear() { base.clear(); }
}

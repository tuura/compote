/*
  Author: Andrey Mokhov, Newcastle University
  Date: 21 December 2012
  Contact: andrey.mokhov@ncl.ac.uk
  Description: Haskell BDD interface.
*/

#include <cassert>
#include "bdd.h"

using namespace bdd;
BDD base;

extern "C"
{
	NodeID one () { return base.referenceNodeID(base.one);  }
	NodeID zero() { return base.referenceNodeID(base.zero); }

	NodeID variable(int v) { return base.variable(v); }

	NodeID ite(NodeID f, NodeID g, NodeID h) { return base.ite(f, g, h); }

	NodeID andGate(NodeID f, NodeID g) { return base.andGate(f, g); }
	NodeID  orGate(NodeID f, NodeID g) { return base. orGate(f, g); }
	NodeID xorGate(NodeID f, NodeID g) { return base.xorGate(f, g); }
	NodeID notGate(NodeID f          ) { return base.notGate(f);    }

	int iteTrue(NodeID f, NodeID g, NodeID h) { return (int)(base.iteConst(f, g, h) == base.one); }
	int imply(NodeID f, NodeID g) { return (int)base.imply(f, g); }

	void dereference(NodeID node) { base.dereferenceNodeID(node); }

	void setCacheSize(size_t cacheSize) { base.setCacheSize(cacheSize); }

	void performGC() { base.performGC(); }
}

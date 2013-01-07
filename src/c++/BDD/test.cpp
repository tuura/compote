/*
	Author: Andrey Mokhov, Newcastle University
	Date: 19 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Testing BDD implementation.
*/

#include <cstdio>
#include <string>
#include "base.h"
#include "node.h"

using namespace bdd;

Base b;

int cnt = 0;

#define test(condition) (report(condition) || error(#condition, __LINE__))

bool report(bool condition)
{
	cnt++;
	printf("Test %02d: ", cnt);
	if (condition) puts("OK"); else puts("FAIL!");
	return condition;
}

bool error(std::string condition, int line)
{
	printf("%s, at line %d\n", condition.c_str(), line);
	return true;
}

int main()
{
	test(isPositive(b.one));
	test(isNegative(b.zero));
	test(b.one == inverted(b.zero));

	NodeID x = b.variable(0);
	NodeID y = b.variable(1);
	NodeID z = b.variable(2);

	test(isNegative(x));
	test(getNodePtr(x)->var == 0);
	test(getNodePtr(x)->low == b.one);
	test(getNodePtr(x)->high == b.zero);
	test(x == b.dereferenceNodeID(b.ite(x, b.one, b.zero)));

	NodeID nx = b.referenceNodeID(inverted(x));
	NodeID ny = b.referenceNodeID(inverted(y));
	NodeID nz = b.referenceNodeID(inverted(z));

	test(x == inverted(nx));
	test(y == inverted(ny));
	test(z == inverted(nz));

	test(ny == b.dereferenceNodeID(b.ite(y, b.zero, b.one)));

	NodeID x_and_y = b.ite(x, y, b.zero);
	NodeID nx_or_ny = b.ite(nx, b.one, ny);
	NodeID not_nx_or_ny = b.referenceNodeID(inverted(nx_or_ny));

	test(x_and_y == b.dereferenceNodeID(b.andGate(x, y)));
	test(nx_or_ny == b.dereferenceNodeID(b.orGate(nx, ny)));
	test(x_and_y == not_nx_or_ny);

	b.setCacheSize(1 << 16);

	NodeID xz = b.andGate(x, z);
	NodeID ynz = b.andGate(y, nz);

	NodeID xz_ynz = b.orGate(xz, ynz);
	NodeID xy_xz_ynz = b.orGate(xz_ynz, x_and_y);

	test(xz_ynz == xy_xz_ynz);

	NodeID x_xor_y = b.xorGate(x, y);
	NodeID x_xor_ny = b.xorGate(x, ny);

	test(b.dereferenceNodeID(b.xorGate(x_xor_y, x_xor_ny)) == b.one);

	test(b.iteConst(x_and_y, x, b.one) == b.one);
	test(b.iteConst(nx_or_ny, nx, b.one) != b.zero);
	test(b.iteConst(nx_or_ny, nx, b.one) != b.one);

	test(b.imply(x_and_y, x));
	test(!b.imply(nx_or_ny, nx));

	test(b.size() == 9);
	test(b.sizeDead() == 0);

	b.dereferenceNodeID(x);
	b.dereferenceNodeID(y);
	b.dereferenceNodeID(z);

	b.dereferenceNodeID(nx);
	b.dereferenceNodeID(ny);
	b.dereferenceNodeID(nz);

	b.dereferenceNodeID(x_and_y);
	b.dereferenceNodeID(nx_or_ny);
	b.dereferenceNodeID(not_nx_or_ny);

	b.dereferenceNodeID(xz);
	b.dereferenceNodeID(ynz);

	b.dereferenceNodeID(xz_ynz);
	b.dereferenceNodeID(xy_xz_ynz);

	b.dereferenceNodeID(x_xor_y);
	b.dereferenceNodeID(x_xor_ny);

	b.runGC();

	test(b.size() == 0);
	test(b.sizeDead() == 0);

	b.clear();

	return 0;
}

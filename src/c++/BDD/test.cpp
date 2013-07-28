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

int total = 0;
int failed = 0;

#define test(condition) (report(condition) || error(#condition, __LINE__))

bool report(bool condition)
{
	total++;
	if (!condition) failed++;
	printf("Test %02d: ", total);
	if (condition) puts("OK"); else puts("FAIL!");
	return condition;
}

bool error(std::string condition, int line)
{
	printf("%s, at line %d\n", condition.c_str(), line);
	return true;
}

void test_report()
{
	printf("\n-----------------------\n\n%d of %d tests failed\n\n", failed, total);
}

int main()
{
	test(isPositive(b.one));
	test(isNegative(b.zero));
	test(b.one == invert(b.zero));

	test(b.size() == 0);

	NodeID x = b.variable(0);
	NodeID y = b.variable(1);
	NodeID z = b.variable(2);

	test(b.size() == 3);
	test(getNodePtr(x)->refs == 1);

	test(isNegative(x));
	test(getNodePtr(x)->var == 0);
	test(getNodePtr(x)->low == b.one);
	test(getNodePtr(x)->high == b.zero);
	test(x == b.dereferenceNodeID(b.ite(x, b.one, b.zero)));

	test(b.size() == 3);
	test(b.sizeDead() == 0);

	NodeID nx = b.referenceNodeID(invert(x));
	NodeID ny = b.referenceNodeID(invert(y));
	NodeID nz = b.referenceNodeID(invert(z));

	test(x == invert(nx));
	test(y == invert(ny));
	test(z == invert(nz));

	test(ny == b.dereferenceNodeID(b.ite(y, b.zero, b.one)));

	test(b.size() == 3);
	test(b.sizeDead() == 0);

	NodeID x_and_y = b.ite(x, y, b.zero);
	NodeID nx_or_ny = b.ite(nx, b.one, ny);

	NodeID not_nx_or_ny = b.referenceNodeID(invert(nx_or_ny));

	test(x_and_y == b.dereferenceNodeID(b.andGate(x, y)));
	test(nx_or_ny == b.dereferenceNodeID(b.orGate(nx, ny)));
	test(x_and_y == not_nx_or_ny);

	test(b.size() == 4);
	test(b.sizeDead() == 0);

	b.setCacheSize(1 << 16);

	NodeID xz = b.andGate(x, z);
	NodeID ynz = b.andGate(y, nz);

	test(b.size() == 6);

	NodeID xz_ynz = b.orGate(xz, ynz);

	test(getNodePtr(ynz)->refs == 2);
	test(getNodePtr(xz_ynz)->refs == 1);

	test(b.size() == 8);

	NodeID xy_xz_ynz = b.orGate(xz_ynz, x_and_y);

	test(getNodePtr(xz_ynz)->refs == 2);
	test(getNodePtr(ynz)->refs == 2);
	test(b.size() == 8);
	test(b.sizeDead() == 0);

	test(xz_ynz == xy_xz_ynz);

	NodeID x_xor_y = b.xorGate(x, y);
	NodeID x_xor_ny = b.xorGate(x, ny);

	test(b.size() == 9);
	test(b.sizeDead() == 0);

	test(b.dereferenceNodeID(b.xorGate(x_xor_y, x_xor_ny)) == b.one);

	test(b.iteConst(x_and_y, x, b.one) == b.one);
	test(b.iteConst(nx_or_ny, nx, b.one) != b.zero);
	test(b.iteConst(nx_or_ny, nx, b.one) != b.one);

	test(b.imply(x_and_y, x));
	test(!b.imply(nx_or_ny, nx));

	b.dereferenceNodeID(x);
	b.dereferenceNodeID(y);
	b.dereferenceNodeID(z);

	test(b.size() == 9);
	test(b.sizeDead() == 0);
	test(getNodePtr(x)->refs == 1);
	test(getNodePtr(y)->refs == 4);
	test(getNodePtr(z)->refs == 4);

	b.dereferenceNodeID(nx);
	b.dereferenceNodeID(ny);
	b.dereferenceNodeID(nz);

	b.dereferenceNodeID(x_and_y);
	test(getNodePtr(x_and_y)->refs == 2);
	b.dereferenceNodeID(nx_or_ny);
	test(getNodePtr(x_and_y)->refs == 1);
	b.dereferenceNodeID(not_nx_or_ny);
	test(getNodePtr(x_and_y)->refs == 0);
	test(getNodePtr(y)->refs == 2);

	b.dereferenceNodeID(xz);
	test(getNodePtr(xz)->refs == 0);

	test(getNodePtr(ynz)->refs == 2);
	b.dereferenceNodeID(ynz);
	test(getNodePtr(ynz)->refs == 1);

	test(getNodePtr(z)->refs == 2);

	b.dereferenceNodeID(xz_ynz);
	test(getNodePtr(xz_ynz)->refs == 1);
	test(getNodePtr(xy_xz_ynz)->refs == 1);
	b.dereferenceNodeID(xy_xz_ynz);
	test(getNodePtr(xz_ynz)->refs == 0);
	test(getNodePtr(xy_xz_ynz)->refs == 0);
	test(getNodePtr(ynz)->refs == 0);

	test(getNodePtr(y)->refs == 2);
	test(getNodePtr(z)->refs == 0);

	b.dereferenceNodeID(x_xor_y);
	b.dereferenceNodeID(x_xor_ny);

	test(b.size() == 9);
	test(b.sizeDead() == 9);

	b.runGC();

	test(b.size() == 0);
	test(b.sizeDead() == 0);

	b.clear();

	test_report();

	return 0;
}

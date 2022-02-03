#include <iostream>
#include <string>
#include <cassert>
using namespace std;

int firstDifference(const string a1[], int n1, const string a2[], int n2)
{
    // This function considers only the first n1 elements of the array a1 and the
    // first n2 elements of the array a2.  It returns the position of the first
    // corresponding elements of a1 and a2 that are not equal.  If the arrays are
    // equal up to the point where one or both of them runs out, then return
    // whichever value of n1 or n2 is less than or equal to the other.  If n1 is
    // 0, that means a1 runs out immediately; similarly, for n2 and a2.  If n1 or
    // n2 is negative, act as if it were 0.             

    if (n1 < 0) {
        n1 = 0;
    }

    if (n2 < 0) {
        n2 = 0;
    }
    
    int shortArray;
    if (n1 < n2) {
        shortArray = n1;
    }
    else
        shortArray = n2;

    for (int i = 0; i < shortArray; i++) {
        if (a1[i] != a2[i]) {
            return i;
        }
    }

    return shortArray;
}


bool isContainedIn(const string a1[], int n1, const string a2[], int n2)
{
    // This function considers only the first n1 elements of the array a1 and the
    // first n2 elements of the array a2.  If all n1 elements of a1 appear as
    // elements in a2, in the same order (though not necessarily consecutively),
    // then return true.  Return false if a2 does not so contain a1.  Of course,
    // a sequence of 0 elements is contained in every sequence, even a sequence
    // of 0 elements.  If n1 or n2 is negative, act as if it were 0.

    if (n1 < 0) {
        n1 = 0;
    }

    if (n2 < 0) {
        n2 = 0;
    }

    if (n1 == 0) {
        return true;
    }

    int index = 0;
    for (int i = 0; i < n2; i++) {
        if (a1[index] == a2[i]) {
            index++;
            if (index == n1) {
                return true;
            }
        }
    }
    return false;
}


int main()
{
    // Here are some tests that demonstrate what these functions do.  You may add
    // more if you wish.
    string colors[6] = { "pink", "blue", "white", "red", "black", "blue" };
    string hues[5] = { "pink", "blue", "white", "black", "green" };
    string tint[5] = { "yellow", "blue", "white", "black", "green" };
    assert(firstDifference(colors, 6, hues, 5) == 3);
    assert(firstDifference(colors, 4, hues, 5) == 3);
    assert(firstDifference(colors, 2, hues, 5) == 2);  // equal up to where a1 runs out
    assert(firstDifference(colors, 2, hues, 2) == 2);  // equal up to where both run out
    assert(firstDifference(colors, 5, hues, 1) == 1);  // equal up to where a2 runs out
    assert(firstDifference(colors, 5, hues, 0) == 0);  // equal up to where a2 runs out
    assert(firstDifference(colors, 5, tint, 0) == 0);  // equal up to where a2 runs out
    string little1[4] = { "blue", "white", "black", "blue" };
    assert(isContainedIn(little1, 4, colors, 6));
    string little2[2] = { "red", "white" };
    assert(!isContainedIn(little2, 2, colors, 6));
    string little3[3] = { "blue", "blue", "red" };
    assert(!isContainedIn(little3, 3, colors, 6));
    assert(isContainedIn(hues, 3, colors, 3));
    assert(!isContainedIn(hues, 3, colors, 2));
    assert(isContainedIn(hues, 0, tint, 5));
    assert(isContainedIn(hues, 0, tint, 0));
    cout << "All tests succeeded" << endl;
}
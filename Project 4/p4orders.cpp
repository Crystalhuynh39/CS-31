#include <iostream>
#include <cstring>  // Notice this is NOT <string>; we need it because
                    // isValidUppercaseStateCode uses std::strstr
#include <cctype>
#include <cassert>
using namespace std;

bool isValidUppercaseStateCode(const char stateCode[])
{
    // In a declaration of an array with initialization, you can omit
    // the number of elements and the compiler will count how many items
    // are in the initializer and use that.  For a C string, the count is
    // the number of characters in the initialization string plus one more
    // for the zero byte.
    const char codes[] =
        "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.GU.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MP.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.PR.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (isupper(stateCode[0]) && isupper(stateCode[1]) &&
        stateCode[2] == '\0' && strstr(codes, stateCode) != nullptr);
}

//*************************************
//  hasValidSyntax
//*************************************

bool hasValidSyntax(char orders[])
{
    int NUMBER_OF_CHARS = strlen(orders);

    // An empty order string is well-formed.

    if (NUMBER_OF_CHARS == 0)
        return true;

    // Each iteration of the loop recognizes one state order

    int k = 0;
    while (k != NUMBER_OF_CHARS)
    {
        // The order must start with two letters

        if (!isalpha(orders[k]))
            return false;
        k++;
        if (k == NUMBER_OF_CHARS || !isalpha(orders[k]))
            return false;
        k++;

        // Those letters must be the code for a state

        char state[3] = "XX";
        state[0] = toupper(orders[k - 2]);
        state[1] = toupper(orders[k - 1]);
        if (!isValidUppercaseStateCode(state))
            return false;

        // The state code must be a followed by a digit

        if (k == NUMBER_OF_CHARS || !isdigit(orders[k]))
            return false;
        k++;

        // There might be more digits

        while (k != NUMBER_OF_CHARS && isdigit(orders[k]))
            k++;

        // There must be a status code

        if (k == NUMBER_OF_CHARS || (orders[k] != '+' && orders[k] != '-'))
            return false;
        k++;
    }

    // We get here if we got through the orders without a problem.

    return true;

}

//*************************************
//  countCases
//*************************************

int countCases(char orders[], char status, int& caseCount)
{
    // Define return values

    const int RET_OK = 0;
    const int RET_BAD_SYNTAX = 1;
    const int RET_BAD_STATE_ORDER = 2;
    const int RET_BAD_STATUS = 3;

    // A bad status character prevents counting

    if (status != '+' && status != '-')
        return RET_BAD_STATUS;

    // An order string with incorrect syntax prevents counting

    if (!hasValidSyntax(orders))
        return RET_BAD_SYNTAX;

    // We will count cases in an int named result, and modify the caseCount
    // parameter only if processing the entire order string succeeds.

    int result = 0;

    // Each iteration of the loop deals with one state order.  Since we
    // know at this point the order string has correct syntax, we are
    // guaranteed there are digits, etc.

    int k = 0;
    while (k != strlen(orders))
    {
        // Skip over the state code (we know there must be one)

        k += 2;

        // Determine the state case count

        int stateCaseCount = 0;
        while (isdigit(orders[k]))  // we know there's at least one digit
        {
            stateCaseCount = 10 * stateCaseCount + orders[k] - '0';
            k++;
        }

        // The state order must not specify zero cases

        if (stateCaseCount == 0)
            return RET_BAD_STATE_ORDER;

        // If the status code (we know there must be one) matches, record
        // the cases 

        if (orders[k] == status)
            result += stateCaseCount;
        k++;
    }

    // We've successfully processed the entire string, so set caseCount.

    caseCount = result;

    return RET_OK;
}

int main()
{
    char test1[100] = "TX38-CA132+";
    char test2[100] = "MX38-CA132+";
    char test3[100] = "TX38-CA132+Ms6-nY290-UT006+MS8+CA15+";
    char test4[100] = "TX38-CA132+";
    char test5[50] = "abcdefg";
    char test6[50] = "CA0+";
    char test7[50] = "tX38-cA132+";
    char test8[25] = "";
    char test9[25] = "TX&CA132+";
    char test10[25] = "CA+";
    assert(hasValidSyntax(test1));
    assert(!hasValidSyntax(test2));
    assert(!hasValidSyntax(test5));
    assert(hasValidSyntax(test6));
    assert(hasValidSyntax(test7));
    assert(hasValidSyntax(test8));
    assert(!hasValidSyntax(test9));
    assert(!hasValidSyntax(test10));
    int cases;

    //    detect whether countCases sets cases
    cases = -999;
    assert(countCases(test3, '+', cases) == 0 && cases == 161);

    //    detect whether countCases leaves cases unchanged
    cases = -999;
    assert(countCases(test4, '%', cases) == 3 && cases == -999);

    cout << "All tests succeeded" << endl;
}
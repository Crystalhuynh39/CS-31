//state code: AL, AK, AZ,AR, CA, CO, CT, DE, DC, FL, GA, GU, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MP, MS, MO, MT, NE, NV, NH, NJ, NM, NY, ND, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY
    //state codes can be (CA, Ca, cA, ca)
//order status is + if filled; - if not filled
//state order: CA132+ or ms6-
//order string: a sequence of 0+ state orders (no spaces i.e.  TX38-CA132+Ms6-nY290-UT006+)

//take an order string and an order status, and compute the total number of filled and unfilled orders

//if parameter orders is not an order string the function returns 1
//if the parameter orders is an order string in which at least one state order specifies 0 cases of masks (GA0+) the function returns 2
//if the parameter status is not + or - then function returns 3
//if any of these situations occur, caseCount must be left unchanged
//if non of these occur, then the function reutrns 0 after setting caseCount to the total number of cases of masks for the state orders in the orders parameter that have the status indicatied bu the status parameter

//countCases may call hasValidSyntax

//functions can't read any input from cin or write output to cout
//program must never access out of range positions in a string



#include <iostream>
#include <string>
#include <cassert>
using namespace std;

//will change letters of the state code to uppercase if not already
bool isValidStateCode(string stateCode)
{
    for (int k = 0; k < stateCode.size(); k++)
    {
        stateCode[k] = toupper(stateCode[k]);
    }

    //will check if stateCode is a valid one
    const string codes =
        "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.GU.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MS.MO.MP.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.PR.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    return (stateCode.size() == 2 &&
        stateCode.find('.') == string::npos &&  // no '.' in stateCode
        codes.find(stateCode) != string::npos);  // match found
}


bool hasValidSyntax(string orders)
{
    //for empty string
    if (orders.size() == 0)
    {
        return true;
    }

    for (int k = 0; k < orders.size(); k++)
    {
        string statecode = orders.substr(k, 2);

        //tests first two characters of the orders string if they are valid state codes
        if (!isValidStateCode(statecode))
        {
            return false;
        }
        k += 2;

        if (k < orders.size()) 
        {
            if (!isdigit(orders[k])) 
            {
                return false;
            }
        }
        else {
            return false;
        }

        //test if characters after the statecode and before '+' or '-' are numbers
        while (k < orders.size() && orders[k] != '+' && orders[k] != '-')
        {
            if (!isdigit(orders[k]))
            {
                return false;
            }

            k++;
        }

        if (k == orders.size())
        {
            return false;
        }
    }
    return true;
}


int countCases(string orders, char status, int& caseCount)
{
    //if parameter orders is not an order string the function returns 1
    if (!hasValidSyntax(orders))
    {
        return 1;
    }

    if (status != '+' && status != '-')
    {
        return 3;
    }

    string OrderCount = "";
    int filledOrderCount = 0;
    int unfilledOrderCount = 0;

    //calculates the amount of filled and unfilled orders based on whether the order status in each state order is a "+" or "-"
    for (int k = 0; k < orders.size(); k++)
    {
        if (isdigit(orders[k]))
        {
            OrderCount += orders[k];
        }

        else if (orders[k] == '+' || orders[k] == '-')
        {
            int digits;
            digits = stoi(OrderCount);
            //checks for 0 orders
            if (digits == 0)
            {
                cout << k << endl;
                return 2;
            }

            if (orders[k] == '+')
            {
                filledOrderCount += digits;
            }
            else
            {
                unfilledOrderCount += digits;
            }
            OrderCount = "";
        }
    }


    //counts filled orders
    if (status == '+')
    {
        caseCount = filledOrderCount;
    }

    //counts unfilled orders
    else
    {
        caseCount = unfilledOrderCount;
    }

    return 0;
}


int main() 
{
    assert(hasValidSyntax("TX38-CA132+"));
    assert(hasValidSyntax("tx38-ca132+"));
    assert(hasValidSyntax("Tx38-Ca132+"));
    assert(hasValidSyntax("tX38-cA132+"));
    assert(hasValidSyntax("CA0+"));
    assert(hasValidSyntax("CA132+tx38-Fl38+dC14-"));
    assert(hasValidSyntax(""));

    assert(!hasValidSyntax("MX38-CA132+"));
    assert(!hasValidSyntax("CA 132+"));
    assert(!hasValidSyntax("CA"));
    assert(!hasValidSyntax("CA132"));
    assert(!hasValidSyntax("CA132 +"));
    assert(!hasValidSyntax("CA132&"));
    assert(!hasValidSyntax("CA 132 +"));
    assert(!hasValidSyntax("TX38- CA132+"));
    assert(!hasValidSyntax("   "));
    assert(!hasValidSyntax("132CA+"));
    assert(!hasValidSyntax("+132CA"));
    assert(!hasValidSyntax("CAC132+"));
    assert(!hasValidSyntax("CA132++"));
    assert(!hasValidSyntax("TX&CA132+"));
    assert(!hasValidSyntax("CA+"));

    int cases;

    //    detect whether countCases sets cases
    cases = -999;
    assert(countCases("TX38-CA132+Ms6-nY290-UT006+MS8+CA15+", '+', cases) == 0 && cases == 161);

    //    detect whether countCases leaves cases unchanged
    cases = -999;
    assert(countCases("TX38-CA132+", '%', cases) == 3 && cases == -999);

    cout << "All tests succeeded" << endl;
}

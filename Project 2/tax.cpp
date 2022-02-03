//the first $50000 of taxable income is taxed at 4%
//the next $70000 of taxable income is taxed at 6%. If the occupation is "engineer" or "scientist" the income in this bracket is taxed at only 5%
//the amount of taxable income that exceeds $120000 is taxed at 9%
//if the taxable income is under $120000, the tax is reduced by $200 per child; however the tax is never allowed to go below zero

#include <iostream>
#include <string>
using namespace std;

int main()
{
	cout << "Name: ";
	string name;
	getline(cin, name);
	if (name == "")																			//for if an empty string was provided for the name
	{
		cout << "---" << endl;
		cout << "You must enter a name" << endl;
		return 1;																			//non-zero return value means program couldn't do its job
	}

	cout << "Taxable income: ";
	double taxableIncome;
	cin >> taxableIncome;
	cin.ignore(1000, '\n');
	if (taxableIncome < 0)																	//for if the taxable income is negative
	{
		cout << "---" << endl;
		cout << "The taxable income must be nonnegative" << endl;
		return 1;																			//non-zero return value means program couldn't do its job
	}

	cout << "Occupation: ";
	string occupation;
	getline(cin, occupation);
	if (occupation == "")																	//for if an empty string was provided for the occupation
	{
		cout << "---" << endl;
		cout << "You must enter an occupation" << endl;
		return 1;																			//non-zero return value means program couldn't do its job
	}

	cout << "Number of children: ";
	double children;
	cin >> children;
	if (children < 0)																		//for if the number of children is negative
	{
		cout << "---" << endl;
		cout << "The number of children must be nonnegative" << endl;
		return 1;																			//non-zero return value means program couldn't do its job
	}

	double pretax;
	if (0 <= taxableIncome && taxableIncome <= 50000)										//to calculate the tax for the first $50000 of taxable income which is taxed at 4%
		pretax = (taxableIncome * 0.04);
	else if (50000 < taxableIncome && taxableIncome <= 120000)								// to calculate the tax for the next $70000 of taxable income
	{
		if (occupation == "engineer")														//if user is an "engineer", then this tax bracket is taxed at 5%
			pretax = ((50000 * 0.04) + ((taxableIncome - 50000) * 0.05));
		else if (occupation == "scientist")													//if user is a "scientist", then this tax bracket is also taxed at 5%
			pretax = ((50000 * 0.04) + ((taxableIncome - 50000) * 0.05));
		else																				//if user has any other occupation besides "engineer" or "scientist", then this tax bracket is taxed at 6%
			pretax = ((50000 * 0.04) + ((taxableIncome - 50000) * 0.06));
	}
	else																					//to calculate the tax if the taxable income exceeds $120000
	{
		if (occupation == "engineer")														//if user is an "engineer", then second tax bracket is taxed at 5%
			pretax = ((50000 * 0.04) + (700000 * 0.05) + ((taxableIncome - 120000) * 0.09));
		else if (occupation == "scientist")													//if user is an "engineer", then second tax bracket is taxed at 5%
			pretax = ((50000 * 0.04) + (700000 * 0.05) + ((taxableIncome - 120000) * 0.09));
		else																				//if user has any other occupation besides "engineer" or "scientist", then second tax bracket is taxed at 6%
			pretax = ((50000 * 0.04) + (700000 * 0.06) + ((taxableIncome - 120000) * 0.09));
	}

	double tax;
	cout.setf(ios::fixed);
	cout.precision(2);																		//sets the numeric layout for the tax in the form $xxxx.xx with two digits to the right of the decimal point
	if (0 <= taxableIncome && taxableIncome < 120000)										//to calculate the tax if the taxable income is under $120000; the tax is reduced by $200 per child
	{
		if ((pretax - (200 * children)) < 0)												//if the tax after the reduction ends up being a negative value, then the tax amount the user would pay is $0.00
			tax = 0;
		else																				//to calculate the tax with the reduction given that it ends up being a positive value
			tax = (pretax - (200 * children));
	}
	else																					//if the taxable income is over $120000, then the tax is not applicable to be reduced
		tax = (pretax);

	cout << "---" << endl;
	cout << name << " would pay $" << tax;

 }
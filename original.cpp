// Code for Project 1
// Report poll results

#include <iostream>
using namespace std;

int main()
{
	int numberSurveyed;
	int obey;
	int disobey;

	cout << "How many people were surveyed? ";
	cin >> numberSurveyed;
	cout << "How many of them are obeying the stay-at-home and other health orders? ";
	cin >> obey;
	cout << "How many of them are disobeying the stay-at-home and other health orders? ";
	cin >> disobey;

	double pctObey = 100.0 * obey / numberSurveyed;
	double pctDisobey = 100.0 * disobey / numberSurveyed;

	cout.setf(ios::fixed);
	cout.precision(1);

	cout << endl;
	cout << pctObey << "% say they are obeying the orders." << endl;
	cout << pctDisobey << "% say they are disobeying the orders." << endl;

	if (obey > disobey)
		cout << "More people are obeying than disobeying the orders." << endl;
	else
		cout << "More people are disobeying than obeying the orders." << endl;
}
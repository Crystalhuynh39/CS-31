//Let's define a simple typo of a string as one of the following:
//adding exactly one character to the string(e.g., adding one character to forty to produce fourty)
//removing exactly one character from the string(e.g., removing a character from february to produce febuary)
//replacing exactly one character in the string(e.g., replacing a character in minuscule to produce miniscule)
//swapping two adjacent characters in the string(e.g., swapping two adjacent characters of pointer to produce poitner)

//The typo score between two strings is defined to be
//0 if the strings are identical
//1 if exactly one simple typo would transform one string to the other
//2 if neither of the above two conditions hold

#include <iostream>
#include <string>
#include <cassert>
using namespace std;

bool addChar(string strDictionary, string strWord);
bool removeChar(string strDictionary, string strWord);
bool replaceChar(string strDictionary, string strWord);
bool swapChar(string strDictionary, string strWord);

int scoreTypo(const string dictionary[], int n, string word)
{
    if (n < 1)
    {
        return -1;
    }

    int lowestTypoScore = 2;                //type score is 2 if neither of the other two conditions hold
    int wrongLetter = 0;

    for (int i = 0; i < n; i++)
    {
        dictionary[i];
        wrongLetter = 0;                              

        if (word == dictionary[i])
        {
            return 0;                        //typo score is 0 if the strings are identical
        }

        //typo score is 1 if exactly one simple typo would transform one string to the other
        if (addChar(dictionary[i], word))
        {
            wrongLetter++;
        }
        if (removeChar(dictionary[i], word))
        {
            wrongLetter++;
        }
        if (replaceChar(dictionary[i], word))
        {
            wrongLetter++;
        }
        if (swapChar(dictionary[i], word))
        {
            wrongLetter++;
        }

        if (wrongLetter == 1)
        {
            lowestTypoScore = 1;
        }
    }

    return lowestTypoScore;
}

//Test to see if there was exactly one character added to the string(e.g., adding one character to forty to produce fourty)
bool addChar(string strDictionary, string strWord)
{
    int addedLetters = 0;

    if (strDictionary.size() + 1 == strWord.size())
    {
        for (int i = 0, j = 0; j < strWord.size(); i++, j++)
        {
            if (strDictionary[i] != strWord[j])
            {
                addedLetters++;
                i--;            
            }
            if (addedLetters > 1)
            {
                break;
            }
        }
    }
    else
    {
        return false;
    }

    if (addedLetters == 1)
    {
        return true;
    }

    return false;
}

//Test to see if there was exactly one character removed from the string(e.g., removing a character from february to produce febuary)
bool removeChar(string strDictionary, string strWord)
{
    int removedLetters = 0;

    if (strDictionary.size() - 1 == strWord.size())
    {
        for (int i = 0, j = 0; j < strWord.size(); i++, j++)
        {
            if (strDictionary[i] != strWord[j])
            {
                removedLetters++;
                j--;            
            }
            if (removedLetters > 1)
            {
                break;
            }
        }
    }
    else
    {
        return false;
    }

    if (removedLetters == 1)
    {
        return true;
    }

    return false;
}

//Tests to see if there was exactly one character replaced in the string(e.g., replacing a character in minuscule to produce miniscule)
bool replaceChar(string strDictionary, string strWord)
{
    int replacedLetters = 0;

    if (strDictionary.size() == strWord.size())
    {
        for (int i = 0; i < strWord.size(); i++)
        {
            if (strDictionary[i] != strWord[i])
            {
                replacedLetters++;
            }
            if (replacedLetters > 1)
            {
                break;
            }
        }
    }
    else
    {
        return false;
    }

    if (replacedLetters == 1)
    {
        return true;
    }
    
    return false;
}

//Test to see if there was swapping of two adjacent characters in the string(e.g., swapping two adjacent characters of pointer to produce poitner)
bool swapChar(string strDictionary, string strWord)
{
    int swappedLetters = 0;
    int wrongLetter = 0;

    if (strDictionary.size() == strWord.size())
    {
        for (int i = 0; i < (strWord.size() - 1); i++)   
        {
            if (strDictionary[i] == strWord[i + 1] && strDictionary[i + 1] == strWord[i])
            {
                swappedLetters++;
            }
            if (strDictionary[i] != strWord[i])
            {
                wrongLetter++;
            }
            if (swappedLetters > 1 || wrongLetter > 2)
            {
                break;
            }
        }
    }
    else
    {
        return false;
    }

    if (swappedLetters == 1)
    {
        return true;
    }

    return false;
}

int main()
{
    // Here are some tests.  You may add more if you wish.
    string dict1[6] = { "february", "pointer", "country", "forty", "conversation", "minuscule" };
    assert(scoreTypo(dict1, 0, "forty") == -1);
    assert(scoreTypo(dict1, 6, "forty") == 0);
    assert(scoreTypo(dict1, 6, "fourty") == 1);
    assert(scoreTypo(dict1, 6, "febuary") == 1);
    assert(scoreTypo(dict1, 6, "miniscule") == 1);
    assert(scoreTypo(dict1, 6, "poitner") == 1);
    assert(scoreTypo(dict1, 6, "conservation") == 2);
    cout << "All tests succeeded" << endl;
}
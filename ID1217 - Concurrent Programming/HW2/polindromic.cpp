#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <vector>
#include <algorithm>
#include <omp.h>
#include <unordered_set>

/* Finding polindromic words using OpenMP

   usage with g++ (version 4.8.4 or higher required):
     g++ -o poli polindromic.cpp -fopenmp -std=c++11 
     ./poli pathToFile numThreads

     Written by : Konstantin Sozinov

*/

using namespace std;

double start_time, end_time; // benchmark
int numThreads; // number of threads

int main(int argc, char *argv[]){
    

    //Set up data structures
    ifstream file(argv[1]);
    ofstream polindroms_file("polindroms.txt");
    numThreads = atoi(argv[2]);
    vector<string> lines;
    unordered_set<string> words;
    string line;
    unordered_set<string> polindroms;
    
    //Insert words into data structures 
    while (getline(file, line)){
        transform(line.begin(), line.end(), line.begin(), ::tolower);
        lines.push_back(line);
        words.insert(line);
    }
    
    cout << "Amout of words : " << words.size() << endl;

    omp_set_num_threads(numThreads);

    start_time = omp_get_wtime();

// Parallel region starts here
#pragma omp parallel for
    for(int i= 0; i < lines.size();i++){
        string word = lines.at(i);
        string reversed = lines.at(i);
        reverse(reversed.begin(),reversed.end());
        if(reversed.compare(word) == 0 || words.count(reversed) > 0){
            // Lock polindroms set in order to update it
            #pragma omp critical
            polindroms.insert(word);
        }
    }



    end_time = omp_get_wtime();

    // Write polindomic words to a file
    for(const string& s:polindroms){
        polindroms_file << s << endl;
    }

    cout << "Amount of polindromic words: " << polindroms.size() << endl;
    printf("It took %g seconds\n", end_time - start_time);

    return 0;
}
#include <iostream>
#include <fstream>
using namespace std;


void arrToFile(double **result, int dimx, int dimy){
	ofstream file;
	file.open("Output.dat");
	for(int j=0;j<dimy;j++){
		for(int i=0; i<dimx;i++){
			file << result[i][j]<<"\t";
		}
		file<<"\n";
	}
}
void printArray2D(double **array,int dimx, int dimy){
	for(int j=0;j<dimy;j++){
		for(int i=0; i<dimx;i++){
			cout << array[i][j]<<"\t";
		}
		cout << "\n";
	}
}

int main(){
	//array decleration
	int dimx=50,dimy=50;
	double** result= new double*[dimx];
	for(int i=0;i<dimy;i++){
		result[i]=new double[dimy];
	}
	//printArray2D(result,dimx,dimy);
}

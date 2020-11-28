#include <iostream>
#include <fstream>
#include <cmath>
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

void matrixMultiplication(double **a,double **b,double **mult, int r1, int c1, int c2){
	for(int i = 0; i < r1; ++i)
        for(int j = 0; j < c2; ++j)
            for(int k = 0; k < c1; ++k)
            {
                mult[i][j] += a[i][k] * b[k][j];
            }
}

void deleteArray(double **array, int dimy){
	for(int i=0; i<dimy;i++){
		delete[] array[i];
	}
	delete[] array;
}

int main(){
	//array decleration

	//matrix[dimx][dimy]
	int dimx=1000,dimy=1000;
	double e=0.0,m=1.0,alpha=1,nu=0;

	double **result= new double*[dimx];
	for(int i=0;i<dimy;i++){
		result[i]=new double[dimy];
	}


//matrix a[2][2]
	double **a= new double*[2];
	for(int i=0;i<2;i++){
		a[i]=new double[2];
	}

//matrix b[2][2]
	double **b= new double*[2];
	for(int i=0;i<2;i++){
		b[i]=new double[2];
	}

//matrix mult[2][2]
	double **mult = new double*[2];
	for(int i=0;i<2;i++){
		mult[i]=new double[2];
	}

	// Initializing a matrix
	a[0][0] = e -2*cos(2*M_PI*m*alpha-nu); a[0][1] =-1.0;
	a[1][0] = 1													 ; a[1][1] = 0.0;




	//Delete everything (Not always needed for newer systems)
	deleteArray(result, dimy);
	deleteArray(a,2);
	deleteArray(b,2);
	deleteArray(mult,2);
}

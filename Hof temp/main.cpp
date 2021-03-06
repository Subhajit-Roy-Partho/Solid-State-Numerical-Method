#include <iostream>
#include <fstream>
#include <cmath>
using namespace std;

int qmax=50;
double nu=0, alpha=0;

//[y][x];
double trace(double **array,int dim){
	double res=0;
	for (int i = 0; i < dim; i++) {
		res+= array[i][i];
	}
	return res;
}

void gnuplotFile(double **result,double *alpha_mat,int dimx, int countAlpha){
	ofstream file;
	file.open("gnuOut.txt");
	for(int j=0; j<countAlpha;j++){
		for(int i=0; i<dimx;i++){
			if(result[j][i]<=4){
				file<< (-4.0) + (i*0.01)<< "\t" << alpha_mat[j]<<"\n";
			}
		}
	}
	file.close();
}

void arrToFile(double **result, int dimx, int dimy){
	ofstream file;
	file.open("Output.dat");
	for(int i=0;i<dimy;i++){
		for(int j=0; j<dimx;j++){
			file << result[i][j]<<"\t";
		}
		file<<"\n";
	}
	file.close();
}
void printArray2D(double **array,int dimx, int dimy){
	for(int i=0;i<dimy;i++){
		for(int j=0; j<dimx;j++){
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

void equateMatrix(double **a, double **b,int dimy, int dimx){
	for(int i=0;i<dimy;i++){
		for(int j=0;j<dimx;j++){
			a[i][j]=b[i][j];
		}
	}
}

void deleteArray(double **array, int dimy){
	for(int i=0; i<dimy;i++){
		delete[] array[i];
	}
	delete[] array;
}

// void multiplicationNtimes(double **a, double **b, double **mult, int dimx, int dimy, int N, double e,double alpha){
// 	a[0][0] = e -2*cos(2*M_PI*m*alpha-nu); a[0][1] =-1.0;
// 	a[1][0] = 1.0; a[1][1] = 0.0;
// 	b[0][0] = e -2*cos(2*M_PI*(m+1)*alpha - nu); b[0][1] =-1.0;
// 	b[1][0]=1.0; b[1][1]=0.0;
// 	for(int i=0; i<N;i++){
// 		matrixMultiplication(a,b,mult,2,2,2);
// 		m+=1;
// 		b[0][0] = e -2*cos(2*M_PI*(m+1)*alpha - nu);
// 		equateMatrix(a,mult,2,2);
// 	}
// }


int main(){
	//array decleration

	//matrix[dimy][dimx]
	int dimx=800,dimy=1000;
	double e=0.0, alpha_mat[dimy]={};//for alpha value store;

	double **result= new double*[dimy];
	for(int i=0;i<dimy;i++){
		result[i]=new double[dimx];
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
	// a[0][0] = e -2*cos(2*M_PI*m*alpha-nu); a[0][1] =-1.0;
	// a[1][0] = 1.0; a[1][1] = 0.0;
	//
	// b[0][0] = e -2*cos(2*M_PI*(m+1)*alpha - nu); b[0][1] =-1.0;
	// b[1][0]=1.0; b[1][1]=0.0;

	//matrixMultiplication(a,b,mult,2,2,2);
	//printArray2D(a,2,2);
	//printArray2D(b,2,2);
	//printArray2D(mult,2,2);


	//main loop
	// for(e=-4; e<=4;e+=0.01)
	// {
	// 	result[0][count]=trace(a,2);
	// 	count+=1;
	// }

// int countAlpha=0;

	// for(int q=1;q<=qmax;q++){
	// 	for(int p=1; p<q; p++){
	// 		alpha = (double)p/(double)q;
	// 		alpha_mat[countAlpha]=alpha;
	// 		countAlpha+=1;
	// 		int countE=0;
	// 		for (double e = -4; e < 4; e+=0.01) {
	// 			if(q>1)
	// 				multiplicationNtimes(a,b,mult,2,2,q,e,alpha);
	// 			result[countAlpha][countE] = abs(trace(a,2));
	// 			countE+=1;
	// 		}
	// 	}
	// }






int countAlpha =0;
cout << "Alpha Values are:\n";

for(int q=1;q<=qmax;q++){
	for(int p=1;p<q+1;p++){
		bool br=false;
		alpha = (double)p/(double)q;
		for(int i=0;i<countAlpha-1;i++){
			if(alpha == alpha_mat[i]){
				// cout << "skipped"<<"\n";
				br=true;
				break;
			}
		}
		if(br){
			// cout<<"Broken\n"; // Just don't do anything;
		}else{
			// cout<< alpha<<"\n";
			countAlpha+=1;
			alpha_mat[countAlpha-1]=alpha;
			int countE=0;
			for(double e =-4.0; e<=4.0;e+=0.01){
				int m=1;
				a[0][0] = e -2*cos(2*M_PI*m*alpha-nu); a[0][1] =-1.0;
				a[1][0] = 1.0; a[1][1] = 0.0;

				b[0][0] = e -2*cos(2*M_PI*(m+1)*alpha - nu); b[0][1] =-1.0;
				b[1][0]=1.0; b[1][1]=0.0;

				mult[0][0]=0.0; mult[0][1]=0.0;
				mult[1][0]=0.0; mult[1][1]=0.0;

				for(int j=1; j<q;j++){ //will work if q >1; Multiplying matrix q times.
					matrixMultiplication(a,b,mult,2,2,2);
					equateMatrix(a,mult,2,2);
					m+=1;
					b[0][0]=e -2*cos(2*M_PI*(m+1)*alpha - nu);
				}
				// cout << "Q = "<<q<<" e = "<<e<< " alpha = "<< alpha<<" trace = "<< trace(a,2)<<"\n";
				// printArray2D(a,2,2);
				result[countAlpha-1][countE]= abs(trace(a,2));
				countE+=1;
			}
		}
	}
}





	cout <<"Total Number of alpha components are "<< countAlpha<<"\n";

	arrToFile(result,dimx,countAlpha);
	gnuplotFile(result,alpha_mat,dimx,countAlpha);
	//Delete everything (Not always needed for newer systems)
	deleteArray(result, dimy);
	deleteArray(a,2);
	deleteArray(b,2);
	deleteArray(mult,2);
	cout << "\n";
}

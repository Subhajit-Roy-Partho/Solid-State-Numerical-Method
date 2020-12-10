#include <iostream>
#include <fstream>
#include <cmath>
#include <time.h>
using namespace std;

//Matrix structure followed in the programme is [y][x]
// [y dimension][x dimension]

int qmax=50; //Maximum value of q which will be itterated.
double nu=0, alpha=0; // alpha = p/q.

//This function is to compute trace of a matrix.
//Format [2-D array, dimension x or dimension y]
double trace(double **array,int dim){
	double res=0;
	for (int i = 0; i < dim; i++) {
		res+= array[i][i];
	}
	return res;
}

//For printing 2d Array with dimensions dimx and dimy
//Format [2-D array, dimension x, dimension y]
void printArray2D(double **array,int dimx, int dimy){
	for(int i=0;i<dimy;i++){
		for(int j=0; j<dimx;j++){
			cout << array[i][j]<<"\t";
		}
		cout << "\n";
	}
}

//Multiplying two matrix a and b and storing the result in matrix mult. Mult should be initialised to 0
//otherwise there might be error
//Format [2-D array a, 2-D array b, 2-D array mult, dimension row1, dimension common, dimension column2]

void matrixMultiplication(double **a,double **b,double **mult, int r1, int c1, int c2){
	for(int i = 0; i < r1; ++i)
        for(int j = 0; j < c2; ++j)
            for(int k = 0; k < c1; ++k)
            {
                mult[i][j] += a[i][k] * b[k][j];
            }
}

// Setting all the values of matrix b to a.
//Format [2-D array a, 2-D array b, dimension y, dimension x]
void equateMatrix(double **a, double **b,int dimy, int dimx){
	for(int i=0;i<dimy;i++){
		for(int j=0;j<dimx;j++){
			a[i][j]=b[i][j];
		}
	}
}

//Deleting array initialised using new.
// Format [array, dimension y]
void deleteArray(double **array, int dimy){
	for(int i=0; i<dimy;i++){
		delete[] array[i];
	}
	delete[] array;
}


void gnuplot(){
  ofstream file2;
  file2.open("gnuscript.gnuplot");
  file2 << "set xlabel 'e'\n set ylabel 'alpha'\n plot 'gnuOut.txt' w p pt 7 ps 0.2";
  file2.close();
  system("gnuplot -p gnuscript.gnuplot");
  system("rm -rf gnuscript.gnuplot");
}

int main(){ //main loop
	double randomness =100.000;
	srand(time(0)); //Seed for the random number which is the current system time.
  // Taking input for maximum value of q
  cout << "Please enter Maximum value of q"<<"\n";
  cin >> qmax;

  //Opening file for storing values for plotting
	ofstream file;
	file.open("gnuOut.txt");




  //array decleration

	//matrix[dimy][dimx]
	int dimx=800,dimy=(int)(qmax*(qmax))/2;
	double e=0.0;//for alpha value store;

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

  //matrix alpha_mat
  double *alpha_mat = new double(dimy);


  int countAlpha =0; //Stores total number of alpha
  cout << "\n";
	int random = 0;
  //Main loop for the calculation of the Hofstadter Butterfly

  for(int q2=1;q2<=qmax;q2++){ // q values going from 1 to qmax
		// random= (double)(rand()%20000 -10000)/100.0;
		int q = q2;
		// cout << q2<<"\n";
  	for(int p2=0;p2<q2+1;p2++){ // p starting from 0 to qmax-1
			cout << "q2 = " << q2 << "\tp2= "<<p2<<"\n";
			int p =p2;
  		bool br=false;
			random = (double)(rand()%20000 -10000)/100.0;
			// q *= random;
			// p = p*random+q;
  		alpha = (double)p/(double)q + (double)1.0/random; //alpha value
      //preventing repetation
  		for(int i=0;i<countAlpha-1;i++){
  			if(alpha == alpha_mat[i]){
  				br=true;//skpi if repeated
  				break;
  			}
  		}
  		if(br){
  			// Do noting if repeated;
  		}else{
  			// Continue for non-repetating values
  			countAlpha+=1;// count for alpha values
  			alpha_mat[countAlpha-1]=alpha;// storing alpha values
  			int countE=0;// Count for Energy index
  			nu = M_PI/(2*(double)(random*q*p)/(double)(random*p+q));// Setting value of nu.
  			for(double e =-4.0; e<=4.0;e+=0.01){
  				int m=1;// Initializing m to 1

          //Initializing all the matrix initial values
  				a[0][0] = e -2*cos(2*M_PI*m*alpha-nu); a[0][1] =-1.0;
  				a[1][0] = 1.0; a[1][1] = 0.0;

  				b[0][0] = e -2*cos(2*M_PI*(m+1)*alpha - nu); b[0][1] =-1.0;
  				b[1][0]=1.0; b[1][1]=0.0;

  				mult[0][0]=0.0; mult[0][1]=0.0;
  				mult[1][0]=0.0; mult[1][1]=0.0;



  				for(int j=1; j<q2;j++){ //will work if q >1; Multiplying matrix q times.
            //Initializing values of mult = 0;

  					mult[0][0]=0.0; mult[0][1]=0.0;
  					mult[1][0]=0.0; mult[1][1]=0.0;
  					matrixMultiplication(a,b,mult,2,2,2); // matrix multiplication
  					equateMatrix(a,mult,2,2);// transfering values of mult to a
  					m+=1;// Increasing the value of m
  					b[0][0]=e -2*cos(2*M_PI*(m+1)*alpha - nu); //defining new value of b
  				}
  				if(abs(trace(a,2))<=4)// If trace value is less than 4 then only record the value
  					file<<e<<"\t"<<alpha<<"\n";// storing to file.

  			}
  		}
  	}
  }





	cout <<"Total Number of alpha components are "<< countAlpha<<"\n";
	file.close();//Closing file gnuOut.txt

  //Deling all array
	deleteArray(a,2);
	deleteArray(b,2);
	deleteArray(mult,2);
	cout << "\n";
  // gnuplot(); // Plotting via gnuscript.

}

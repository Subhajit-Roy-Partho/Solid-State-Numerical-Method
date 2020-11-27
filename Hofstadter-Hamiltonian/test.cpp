#include <iostream>
using namespace std;

void print(int **array){
	for(int i=0; i<5;i++){
		for(int j=0;j<5;j++){
			cout << array[i][j]<<"\t";
		}
		cout<< "\n";
	}
}

int main(){
	int row=5, col=5;
	int **a = new int*[row];
	for(int i=0; i<row;i++){
		a[i] = new int[col];
	}
	for(int j=0; j<5;j++){
		for(int i=0;i<5;i++){
			a[j][i] = j*5+i;
		}
	}
	print(a);
}

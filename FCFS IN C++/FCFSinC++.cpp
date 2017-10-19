/** A C++ - LANGUAGE IMPLEMENTATION OF FCFS DISK SCHEDULING ALGORITHM
*/
#include <iostream>
#include <stdlib.h>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
void FCFS(int a[], int st[], int t[], int N, int h, int& cnt, double& ast);
void display(int a[], int N);
using namespace std;

int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE], T[SIZE];
  double AST;
  //start timming
  clock_t startTime = clock();
  printf("***********FCFS DISK SCHEDULING ALGORITHM IN C++ *****************");
  do {
  cout<<"\nRead in the total number of cylinders between 1 and 50: N\n";
  cin>>n;
  }while(n < 1 || n > 50);
  cout<<"\nRead in "<<n<<" disk cylinder numbers or track locations";
  for(i = 0; i < n; i++)
  {
   do
   { cout<<"\nRead in disk cylinder number or track location "<< i<<": between 0 and 199 ";      
     cin>> A[i];  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { cout<<"\nRead in the initial disk head position between 0 and 199 ";      
     cin>>h;  
   }while((h < MIN) || (h > MAX));
  //perform FCFS on the Cylinder numbers or track locations
  FCFS(A, ST, T, n, h, count, AST);
  cout<<"\nThe original list is: \n"; display(A, n);
  cout<<"\nThe New(FCFS) list is: \n"; display(T, n);
  cout<<"\nThe total head movement/seek time is: "<<count;
  cout<<"\nThe Average head movement/seek time is: "<<AST<<endl;
  cout<<"The Execution time is: "<<double(clock() - startTime)/(double) CLOCKS_PER_SEC<<" seconds."<<endl;
  system("PAUSE");	
  return 0;
}
void FCFS(int a[], int st[], int t[], int N, int h, int& cnt, double& ast)
{    int i;
     cnt += abs(h - a[0]);
     st[0] = cnt;  t[0] = a[0];
     for(i = 1; i < N; i++)
     {
      cnt += abs(a[i] - a[i-1]);
      st[i] = cnt;  t[i] = a[i];     
     }
     ast = (double)cnt/N;
     
}
void display(int a[], int N)
{
    int i;
    for(i = 0; i < N; i++)
    cout<<a[i]<<" ";
}

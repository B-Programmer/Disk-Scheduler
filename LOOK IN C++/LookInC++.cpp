/** A C++ - LANGUAGE IMPLEMENTATION OF LOOK DISK SCHEDULING ALGORITHM
*/
#include <cstdlib>
#include <iostream>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50

using namespace std;
void LOOK(int a[], int t[], int N, int h, int& thm, double& ast);
void sort(int a[], int n);
void display(int a[], int N);
int main()
{
  int n, h, THM = 0, i;
  int A[SIZE], T[SIZE];
  double AST;
   //start timming
  clock_t startTime = clock();
  cout<<"******** LOOK DISK SCHEDULING ALGORITHM IN C++ ***********";
  do {
  cout<<"\nRead in the total number of cylinders between 1 and 50: N\n";
  cin>>n;
  }while(n < 1 || n > SIZE);
  cout<<"\nRead in "<<n<<" disk cylinder numbers or track locations";
  for(i = 0; i < n; i++)
  {
   do
   { cout<<"\nRead in disk cylinder number or track location "<<i<<", between 0 and 199: ";      
     cin>>A[i];  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { cout<<"\nRead in the initial disk head position between 0 and 199: ";      
     cin>>h;  
   }while((h < MIN) || (h > MAX));
  //perform SCAN on the Cylinder numbers or track locations
  LOOK(A, T, n, h, THM, AST);
  cout<<"\nThe original LOOK list is: \n"; display(A, n);
  cout<<"\nThe New LOOK list is: \n"; display(T, n);
  cout<<"\nThe total head movement/seek time is: "<<THM;
  cout<<"\nThe Average head movement/seek time is: "<<AST<<endl;
  cout<<"The Execution time is: "<<double(clock() - startTime)/(double) CLOCKS_PER_SEC<<" seconds."<<endl;
  system("PAUSE");	
  return 0;
}
void LOOK(int a[], int t[], int N, int h, int& thm, double& ast)
{    int i, J = 0, L = 0, R = 0, LE[N], RE[N];
     for(i = 0; i < N; i++)
     {
           if(a[i] > h) RE[R++] = a[i]; 
           else
           LE[L++] = a[i];
     }
     sort(LE, L);
     sort(RE, R);
     for(i = L-1; i >= 0; i--)
     {
           thm += abs(h - LE[i]);
           t[J] = LE[i];            
           h = t[J]; J++;
     }      
     for(i = 0; i < R; i++)
     {
           thm += abs(h - RE[i]);
           t[J] = RE[i];            
           h = t[J]; J++;
     }
     ast = (double)thm/N;
     
}
void sort(int a[], int n)
{
     int i, j,tmp;
     for(i = 0; i < n-1; i++)
     {
           for(j = i+1; j < n; j++)
           {
                 if(a[i] > a[j]){ tmp = a[i]; a[i] = a[j]; a[j] = tmp; }
           }
     }      
}
void display(int a[], int N)
{
    int i;
    for(i = 0; i < N; i++)
    cout<< a[i]<<" ";
}

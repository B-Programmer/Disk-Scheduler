/** A C++ - LANGUAGE IMPLEMENTATION OF SCAN DISK SCHEDULING ALGORITHM
*/
#include <cstdlib>
#include <iostream>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50

using namespace std;
void SCAN(int a[], int st[], int t[], int N, int h, int& cnt, double& ast);
void sort(int a[], int n);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE+1], T[SIZE+1];
  double AST;
     //start timming
  clock_t startTime = clock();
  cout<<"*********** SCAN DISK SCHEDULING ALGORITHM IN C++ *****************";
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
  SCAN(A, ST, T, n, h, count, AST);
  cout<<"\nThe original list is: \n"; display(A, n);
  cout<<"\nThe New(SCAN) list is: \n"; display(T, n+1);
  cout<<"\nThe total head movement/seek time is: "<<count;
  cout<<"\nThe Average head movement/seek time is: "<<AST<<endl;
  cout<<"The Execution time is: "<<double(clock() - startTime)/(double) CLOCKS_PER_SEC<<" seconds."<<endl;
  system("PAUSE");	
  return 0;
}
void SCAN(int a[], int st[], int t[], int N, int h, int& cnt, double& ast)
{    int i, J = 0, L = 0, R = 0, Temp, LE[N], RE[N];
     for(i = 0; i < N; i++)
     {
           if(a[i] > h) RE[R++] = a[i]; 
           else
           LE[L++] = a[i];
     }
     sort(LE, L);
     sort(RE, R);
     Temp = h;
     for(i = L-1; i >= 0; i--)
     {
           t[J] = LE[i]; 
           cnt += abs(Temp - t[J]);
           st[J] = cnt;  Temp = t[J]; J++;
     }      
     t[J] = MIN; 
     cnt += abs(LE[0] - t[J]);
      st[J] = cnt;  Temp = t[J]; J++;      
     for(i = 0; i < R; i++)
     {
           t[J] = RE[i]; 
           cnt += abs(Temp - t[J]);
           st[J] = cnt;  Temp = t[J]; J++;
     }
     ast = (double)cnt/N;
     
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

/** A C++ - LANGUAGE IMPLEMENTATION OF C-SCAN DISK SCHEDULING ALGORITHM
*/
#include <cstdlib>
#include <iostream>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50

using namespace std;
void CSCAN(int a[], int st[], int t[], int N, int h, int& cnt, double& ast);
void sort(int a[], int n);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE+2], T[SIZE+2];
  double AST;
   //start timming
  clock_t startTime = clock();
  cout<<"*********** C-SCAN DISK SCHEDULING ALGORITHM IN C++ *****************";
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
  //perform C-SCAN on the Cylinder numbers or track locations
  CSCAN(A, ST, T, n, h, count, AST);
  cout<<"\nThe original list is: \n"; display(A, n);
  cout<<"\nThe New(C-SCAN) list is: \n"; display(T, n+2);
  cout<<"\nThe total head movement/seek time is: "<<count;
  cout<<"\nThe Average head movement/seek time is: "<<AST<<endl;
  cout<<"The Execution time is: "<<double(clock() - startTime)/(double) CLOCKS_PER_SEC<<" seconds."<<endl;
  system("PAUSE");	
  return 0;
}
void CSCAN(int a[], int st[], int t[], int N, int h, int& cnt, double& ast)
{    int i, J = 0, L = 0, R = 0, Temp, Temp1, LE[N], RE[N];
     for(i = 0; i < N; i++)
     {
           if(a[i] > h) RE[R++] = a[i]; 
           else
           LE[L++] = a[i];
     }
     sort(LE, L);
     sort(RE, R);
     Temp = h;
     for(i = 0; i < R; i++)
     {
           t[J] = RE[i]; 
           cnt += abs(Temp - t[J]);
           st[J] = cnt;  Temp = t[J]; J++;
     }
     Temp = MAX; Temp1 = RE[R-1];
     for(i = 0; i <= 1; i++)
     {
           t[J] = Temp; 
           cnt += abs(Temp1 - Temp);
           st[J] = cnt;  Temp1 = Temp; Temp = MIN; J++;      
     }
     for(i = 0; i < L; i++)
     {
           t[J] = LE[i]; 
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

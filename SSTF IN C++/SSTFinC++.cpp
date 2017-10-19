/** A C++ - LANGUAGE IMPLEMENTATION OF SSTF DISK SCHEDULING ALGORITHM
*/
#include <cstdlib>
#include <iostream>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
using namespace std;

void SSTF(int a[], int st[], int t[], int N, int h, int& cnt, double& ast, int d[]);
int getMinDist(int d[], int N);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE], T[SIZE], Dist[SIZE];
  double AST;
  //start timming
  clock_t startTime = clock();
  cout<<"***********SSTF DISK SCHEDULING ALGORITHM IN C++ *****************";
  do {
  cout<<"\nRead in the total number of cylinders between 1 and 50: N\n";
  cin>> n;
  }while(n < 1 || n > SIZE);
  cout<<"\nRead in "<<n<<" disk cylinder numbers or track locations";
  for(i = 0; i < n; i++)
  {
   do
   { cout<<"\nRead in disk cylinder number or track location "<< i<<", between 0 and 199: ";      
     cin>>A[i];  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { cout<<"\nRead in the initial disk head position between 0 and 199: ";      
     cin>> h;  
   }while((h < MIN) || (h > MAX));
  //perform SSTF on the Cylinder numbers or track locations
  SSTF(A, ST, T, n, h, count, AST, Dist);
  cout<<"\nThe original list is: \n"; display(A, n);
  cout<<"\nThe New(SSTF) list is: \n"; display(T, n);
  cout<<"\nThe total head movement/seek time is: "<<count;
  cout<<"\nThe Average head movement/seek time is: "<<AST<<endl;
  cout<<"The Execution time is: "<<double(clock() - startTime)/(double) CLOCKS_PER_SEC<<" seconds."<<endl;
  system("PAUSE");	
  return 0;
}
void SSTF(int a[], int st[], int t[], int N, int h, int& cnt, double& ast, int d[])
{    int i, k, J = 0, L = 0, LCnt = 0, Cnt = 0, Temp, Loc[N];
     int locFound; 
     Temp = h;
     for(k = 0; k < N; k++) Loc[k] = -1;
     while(Cnt < N)
     {
     for(i = 0; i < N; i++)
     {
           locFound = 0;
           for(k = 0; k < N; k++)
           {
                 if(i == Loc[k]){ locFound = 1; break; }
           }/*end for loop */
           if(locFound == 1) d[i] = MAX + 1;
           else 
           d[i] = abs(Temp - a[i]);
     } /*end of for loop */
     L = getMinDist(d, N); 
     Loc[LCnt++] = L; 
      t[J] = a[L]; 
     cnt += abs(Temp - t[J]);
      st[i] = cnt;  Temp = t[J]; J++;      
     Cnt++; 
     }/*end while */
     ast = (double)cnt/N;
     
}
int getMinDist(int d[], int N)
{
    int i, tmp = 0;
    for(i = 1; i < N; i++)
    {
          if(d[tmp] > d[i])tmp = i;
    }
    return tmp;
}
void display(int a[], int N)
{
    int i;
    for(i = 0; i < N; i++)
    cout<< a[i]<<" ";
}


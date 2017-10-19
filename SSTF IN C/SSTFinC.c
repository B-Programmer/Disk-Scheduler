/** A C - LANGUAGE IMPLEMENTATION OF SSTF DISK SCHEDULING ALGORITHM
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#define MAX 199
#define MIN 0
#define SIZE 50
void SSTF(int a[], int st[], int t[], int N, int h, int *cnt, double *ast, int d[]);
int getMinDist(int d[], int N);
void display(int a[], int N);
int main()
{
  int n, h, count = 0, i;
  int A[SIZE], ST[SIZE], T[SIZE], Dist[SIZE];
  double AST;
  //start timming
  clock_t startTiming = clock();
  printf("*********** SSTF DISK SCHEDULING ALGORITHM IN C *****************");
  do {
  printf("\nRead in the total number of cylinders between 1 and 50: N\n");
  scanf("%d",&n);
  }while(n < 1 || n > SIZE);
  printf("\nRead in %d disk cylinder numbers or track locations", n);
  for(i = 0; i < n; i++)
  {
   do
   { printf("\nRead in disk cylinder number or track location %d: between 0 and 199 ", i);      
     scanf("%d", &A[i]);  
   }while((A[i] < MIN) || (A[i] > MAX));
  }
   do
   { printf("\nRead in the initial disk head position between 0 and 199 ");      
     scanf("%d", &h);  
   }while((h < MIN) || (h > MAX));
  //perform SSTF on the Cylinder numbers or track locations
  SSTF(A, ST, T, n, h, &count, &AST, Dist);
  printf("\nThe original list is: \n"); display(A, n);
  printf("\nThe New(SSTF) list is: \n"); display(T, n);
  printf("\nThe total head movement/seek time is: %d", count);
  printf("\nThe Average head movement/seek time is: %.2f\n", AST);
  //End timming
  clock_t EndTiming = clock();
  double ExecutionTime = (double)(EndTiming - startTiming)/CLOCKS_PER_SEC;
  printf("The Execution time is: %.2f seconds.\n", ExecutionTime);
  system("PAUSE");	
  return 0;
}
void SSTF(int a[], int st[], int t[], int N, int h, int *cnt, double *ast, int d[])
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
           d[i] = (int)abs((double)Temp - a[i]);
     } /*end of for loop */
     L = getMinDist(d, N); 
     Loc[LCnt++] = L; 
      t[J] = a[L]; 
     *cnt += (int)abs((double)Temp - t[J]);
      st[i] = *cnt;  Temp = t[J]; J++;      
     Cnt++; 
     }/*end while */
     *ast = (double)*cnt/N;
     
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
    printf("%d ", a[i]);
}

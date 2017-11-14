#include <stdio.h>
#include <omp.h>

omp_set_num_threads(4);
//#pragma omp parallel 
// int ID = omp_get_thread_num();
//#pragma omp parallel num_threads(4)
//#pragma omp critical // only one thread caan enter at a time. Wait their turn
//#pragma omp atomic //provides mutual exclusion but only applies to the update of a memory location
//#pragma omp for //makes the loop variable i private to each thread by default

/* 
	#pragma omp parallel for
	*/
int main()
{
	
}
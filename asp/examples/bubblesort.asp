int bubbleSort(int* arr, int n) 
{ 
	int i;
	int j;
	int temp; 
	bool swapped; 
	bool cont;

	cont = true;

	for (i = 0; i < n - 1 and cont; i++)  
	{ 
		swapped = false; 
		for (j = 0; j < n - i - 1; j++)  
		{ 
			if (arr@j > arr@(j + 1))  
			{ 
				// swap arr[j] and arr[j+1] 
				temp = arr@j; 
				arr@j = arr@(j + 1); 
				arr@(j + 1) = temp; 
				swapped = true; 
			} 
		} 

		// IF no two elements were  
		// swapped by inner loop, then break 
		if (swapped == false) 
			cont = false;
	} 
	return 0;
} 

int main() 
{ 
	int* arr;
	int n;
	int i; 

	n = 7;
	arr = [ 64, 34, 25, 12, 22, 11, 90 ];
	 bubbleSort(arr, n); 

	for (i =0; i < n; i++) {
		print(arr@i);
	}	
	return 0;
} 


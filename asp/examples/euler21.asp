int main() {
	int sum1;
    int sum2;
    int correct;
	
	int num;
	int j;
	int k;

	sum1 = 0;
	sum2 = 0;
	correct =0;
       
    for(num=1; num<10000; num++) {
    	for(j=1; j<num; j++) {
    		if(num%j == 0) {
        		sum1+=j;
        	}
    	}
    	for(k=1; k<sum1; k++) {
    		if(sum1%k == 0) {
    			sum2+=k;
     		}
     	}
       	if(num==sum2 and sum1!=num) {
    		correct+=num;
			print(num);
   		} 
     	sum1=0;
    	sum2=0;
   }

	print("sum:");	
	print(correct);
	return 0;
}

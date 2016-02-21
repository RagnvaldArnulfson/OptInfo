#include <stdio.h>

int main(){
    float a = 1, b = 1;

    while(!((a+b)-a-b))
        a*=2;
    while((a+b)-a-b)
        b+=1;

    printf("a=%f\nb=%f\n",a,b);

    system("pause");
    return 0;
}

#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <utime.h>
#include <sys/stat.h>

#include <linux/sched.h>
#include <linux/tty.h>
#include <linux/kernel.h>
#include <asm/segment.h>
#include <string.h>

/* reference:http://blog.csdn.net/liushuaikobe/article/details/6965859 */

/*以上头文件可能不必全包含，但是我偷懒了，直接全包含算了*/
/*注意：注释时一定不要用双斜线，变量的声明要全写在可执行语句的前面，必须严格按着C语言的语法*/
char myName[23];	/*内核态全局变量，保存从用户态得到的名字*/
int len;		/*内核态全局变量，保存从用户态得到的名字的长度*/	
/*注意：在本文件中声明以及定义的变量均为内核态变量，而且不能直接访问用户态变量*/
/*2011-11-4 19:23*/
int sys_iam(const char *name)
{
	char temp[23];
	int i;
	for (i=0;(temp[i] = get_fs_byte(&name[i]))!='\0';i++){}/*获取名字的长度*/
	if (i>23)
	{
		return (-EINVAL);/*直接这样写可以把指导书上的情况全包含*/
	}
	len = i;
	for (i=0;i<len;i++)
	{
		myName[i] = temp[i];
	}
	
	/*printk("Save complete!\n");*//*内核中向终端输出用printk函数*/
	return len;
}

int sys_whoami(char* name, unsigned int size)
{
	int i;
	if (len>size)
	{
		return (-EINVAL);
	}
	for (i=0;i<len;i++)
	{
		put_fs_byte(myName[i],&name[i]);	
	}
	/*printk("%s\n",myName);*/
	/*printk("Copy complete!\n");*/
	return len;
}

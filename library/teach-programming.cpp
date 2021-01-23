#include <iostream>
#include <string>
#include <cstdlib>
#include <conio.h>
#include <windows.h>
#include <time.h>

#include <iostream>
#include <cstdlib>
#include <string>
#include <windows.h>
#include <conio.h>
using namespace std;

#ifndef _CMDTFUNC_
#define _CMDTFUNC_

#define sHnd GetStdHandle(STD_OUTPUT_HANDLE)

#define FOREGROUND_YELLOW FOREGROUND_RED | FOREGROUND_GREEN
#define FOREGROUND_PINK FOREGROUND_RED | FOREGROUND_BLUE
#define FOREGROUND_CYAN FOREGROUND_GREEN | FOREGROUND_BLUE
#define FOREGROUND_WHITE FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE

#define BACKGROUND_YELLOW BACKGROUND_RED | BACKGROUND_GREEN
#define BACKGROUND_PINK BACKGROUND_RED | BACKGROUND_BLUE
#define BACKGROUND_CYAN BACKGROUND_GREEN | BACKGROUND_BLUE
#define BACKGROUND_WHITE BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE

#define rpush(val) result = result | val

#define ERR_INVAILD_COLOR 2 
#define ERR_INVAILD_COLOR_CHAR 3

void err(string info,int errcode) {
	cout<<info<<endl;
	exit(errcode);
	return;
}

WORD getColorByString(string s) {
	char b=toupper(s[0]),f=toupper(s[1]);
	WORD result = 0;
	switch (f) {
		case '0':
			break;
		case '9':
			rpush(FOREGROUND_INTENSITY);
		case '1':
			rpush(FOREGROUND_BLUE);
			break;
		case 'A':
			rpush(FOREGROUND_INTENSITY);
		case '2':
			rpush(FOREGROUND_GREEN);
			break;
		case 'B':
			rpush(FOREGROUND_INTENSITY);
		case '3':
			rpush(FOREGROUND_CYAN);
			break;
		case 'C':
			rpush(FOREGROUND_INTENSITY);
		case '4':
			rpush(FOREGROUND_RED);
			break;
		case 'D':
			rpush(FOREGROUND_INTENSITY);
		case '5':
			rpush(FOREGROUND_PINK);
			break;
		case 'E':
			rpush(FOREGROUND_INTENSITY);
		case '6':
			rpush(FOREGROUND_YELLOW);
			break;
		case 'F':
			rpush(FOREGROUND_INTENSITY);
		case '7':
			rpush(FOREGROUND_WHITE);
			break;
		case '8':
			err("Invaild color\n",ERR_INVAILD_COLOR);
			break;
		default:
			err("Invaild color\n",ERR_INVAILD_COLOR_CHAR);
			break;	
	}
	switch (b) {
		case '0':
			break;
		case '1':
			rpush(BACKGROUND_BLUE);
		case '9':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '2':
			rpush(BACKGROUND_GREEN);
		case 'A':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '3':
			rpush(BACKGROUND_CYAN);
		case 'B':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '4':
			rpush(BACKGROUND_RED);
		case 'C':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '5':
			rpush(BACKGROUND_PINK);
		case 'D':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '6':
			rpush(BACKGROUND_YELLOW);
		case 'E':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '7':
			rpush(BACKGROUND_WHITE);
		case 'F':
			rpush(BACKGROUND_INTENSITY);
			break;
		case '8':
			err("Invaild color\n",ERR_INVAILD_COLOR);
			break;
		default:
			err("Invaild color\n",ERR_INVAILD_COLOR_CHAR);
			break;	
	}
	return result;
}

void setColor(string s2) {
	SetConsoleTextAttribute(sHnd,getColorByString(s2));
}

#endif 

using namespace std;

string classname[] = {"基础C++","排序","搜索","栈","队列","树","图","并查集"};
int tm,hardlevel[] = {10,20,30,30,30,50,70,70};
int m,stud,sm,taskr[] = {7,5,4,4,2,1,1,1};
int q = 0,power[] = {7,5,4,4,2,1,1,1},tonoipstandard[] = {21,15,12,12,6,3,3,3,20},tonoip[] = {21,15,12,12,6,3,3,3,50};//{21,15,12,12,6,3,3,3,50}
int penalty[8] = {0};

class scRandomHandle {
	public:
		int getRandom() {
			srand(this->seed);
			int rs = rand();
			this->seed = rs;
			return rs;
		}
		void setSeed(int seed) {
			this->seed = seed;
		}
		scRandomHandle() {
		}
		scRandomHandle(int seed) {
			this->seed = seed;
		}
	private:
		int seed;
};

scRandomHandle scrh;
/*
int random(int min,int max) {
 srand(rand());
 return rand()%(max-min)+min;
}*/
int random(int min,int max) {
	return scrh.getRandom()%(max-min)+min;
}

void counting(int sec) {
  cout<<sec;
  Sleep(1000);
  for (int z = sec-1; z > 0; z--) {
  	if (z>8) cout<<"\b\b";
  	if (kbhit()) return; 
  	else cout<<"\b";
  	cout<<z;
  	Sleep(1000);
  } 
}

int main() {
 scrh.setSeed(time(NULL));
 m = 10000;
 stud = 10;
 tm = 1;
 sm = 0;
 int nm = 0,printed = 0;
 bool self_shutdown = false;
 int stud_total = 10;
 while (m>0&&stud) {
  system("cls");
  q+=2;
  tonoip[8]=((stud*(tm+1))-printed);
  cout << "老师教编程" << endl;
  cout << "老师还有 $ " << m << " 和 " << stud << " 个学生，";
  setColor("0B");
  cout << "任务奖励为 $ " << tm * 5000 << "  ";
  setColor("0A");
  cout << "有" << nm << "个学生可以参加CSP！" << endl << endl;
  if ((q+1)%30<=5) cout << "CSP 比赛进行中！" << endl;
  setColor("0E");
  for (int i = 0; i < 8; i++) if (!(q%(i+3))) {
   cout << classname[i] << "的精力增加了!" << endl;
   power[i] += 1;
  }
   setColor("0D");
   int lifemoney = random(90,200);
   cout << "老师要交生活费了，交了 $ " << lifemoney << endl;
   m -= lifemoney;
  for (int i = 0; i < 8; i++) {
  	setColor("07");
   cout << i << " " << classname[i] << endl;
   setColor("0D");
   cout << " 难度 " << hardlevel[i] << "%";
   setColor("0C");
   cout << " 惩罚 " << penalty[i] << "%";
   setColor("0E");
   cout << "    精力 " << power[i] ;
   setColor("0B");
   cout << "  任务剩余 " << taskr[i];
   setColor("0A");
   cout << "  到CSP还需要" << tonoip[i] << endl;
  }
  setColor("07");
  cout << "按 9 - 试卷，试卷每张 $ 100 - ";
  setColor("0A");
  cout << "到 CSP 还需要 " << tonoip[8] << endl;
  setColor("07");
  cout << "请问要让老师教哪门课？(0~9的数字键 8 = 退出 - = 参加CSP)" << endl;
  bool hosi = false;
  char c = getch();
  if (c >= '0' && c < '8') {
   int sel = c - '0';
   if (!power[sel]) {
   	setColor("0E");
   	cout << "老师没有精力教了！" << endl;
   }
   else {
   	
   	setColor("07");
    power[sel]--;
    int ok = random(0,100);
    int pay = random(hardlevel[sel]*4,hardlevel[sel]*8)+stud_total*10;
    if (ok < hardlevel[sel]) {
    setColor("0C");
     cout << "老师翻车了，需要赔偿 $ " << pay << endl;
     m -= pay + ((pay*1.00) * (penalty[sel] * 0.01));
     penalty[sel] += penalty[sel]<100?hardlevel[sel]/10:0;
     if (random(0,100)<70) {
     	cout << "老师因此流失了1个学生。" << endl; 
     	stud--;
	 }
     power[sel]-=2; // god knows why
    } else {
    setColor("0A");
     cout << "老师做对了题目，得到 $ " << pay << endl;
     m += pay - ((pay*1.00) * (penalty[sel] * 0.01));
     penalty[sel] -= penalty[sel]>0?hardlevel[sel]/10:0;
     if (random(0,100)>60) {
     	setColor("0B");
     	cout << "有1个学生因此慕名而来。" << endl;
     	stud++;
	 }
     stud_total++;
     if (taskr[sel]>0) taskr[sel]--;
     if (tonoip[sel]>0) tonoip[sel]--;
    }
    setColor("07");
    if (power[sel]<0) {
    	hosi = true;
    	power[sel]=0;
    	system("cls");
    	int hospital = random(3000,4500);
    	setColor("0F");
    	cout << "老师过度劳累，住进了医院。" << endl << "医院需要支付医药费 $ " << hospital << endl << endl << "老师正在医院里治疗 ..." << endl;
    	counting(15);
    	m -= hospital;
	}
   }
   bool okflag = true,noipflag = true;
   for (int i = 0; i < 9; i++) {
    if (i<8&&taskr[i]) {
     okflag = false;
    }
    if (tonoip[i]) {
     noipflag = false;
    }
   }
   if (okflag) {
   	setColor("0B");
    cout << endl << "老师完成了任务！" << endl;
    cout << "任务奖励获得 $ " << tm * 5000 << " ! " << endl;
    m += tm * 2000;
    tm++;
    for (int i = 0; i < 8; i++) taskr[i]=(80-hardlevel[i])/10*tm;
   }
   if (noipflag) {
   	setColor("0A");
    cout << "现在又有" << tm * 5 << "个学生完成了CSP训练，总计有" << (tm * 5)+nm << endl;
    nm += tm * 5;
    for (int i = 0; i < 9; i++) tonoip[i]=tm*tonoipstandard[i];
   }
  } else if (c == '8') {
   char choice = 'n';
   setColor("07");
   cout << "您真的要退出吗？(y/n)" << endl;
   choice=getch();
   if (choice=='y'||choice=='Y') {
   	self_shutdown = true;
   	break;
   }
  } else if (c == '9') {
  	setColor("07");
   int count;
   cout << "请问要打印几张试卷?" << endl;
   cin >> count;
   m -= count * 100;
   tonoip[8]-=count;
   printed+=count;
   cout << "打印成功，花费$" << count * 100 << "。" << endl;
   bool noipflag = true;
   for (int i = 0; i < 9; i++) {
    if (tonoip[i]) {
     noipflag = false;
    }
   }
   if (noipflag) {
   	setColor("0A");
    cout << "现在又有" << tm * 5 << "个学生完成了CSP训练，总计有" << (tm*5)+nm << endl;
    nm += tm * 5;
    for (int i = 0; i < 9; i++) tonoip[i]=tm*tonoipstandard[i];
   }
  } else if (c == '-') {
  	setColor("0A");
   if (nm<1) cout << "没有人可以参加CSP！" << endl;
   if ((q+1)%30>5) cout << "CSP 比赛还没开始呢！" << endl;
   if (!(nm<1)&&!(((q+1)%30>5))) {
    cout << "参加CSP普及组初赛中......" << endl;
    Sleep(5000);
    int mm = nm / random(1,5), mmm = 0;
    if (mm>0) {
     cout << "还有" << mm << "人可以参加复赛，复赛进行中......" << endl;
     Sleep(10000);
     mmm = mm / random(1,20);
     if (mmm>0) {
      cout << "有" << mm << "个人获得了奖！" << endl;
     }
    }
    m += nm * 2000;
    if (mm>0) m += (mm * 5000);
    if (mmm>0) m += (mmm * 10000);
    cout << "总共赚了 $ " << nm * 2000 << " + " << mm * 5000 << " + " << mmm * 10000 << "！" << endl;
    Sleep(2000);
    nm=0;
    mm=0;
    mmm=0;
   }
  }
  if (!hosi) counting(5);
 }
 setColor("0C");
 cout << "老师最终被迫退出。原因是：" << endl;
 if (m<0) {
  cout << " 资金不足。" << endl;
 } else if (stud <= 0) {
  cout << " 学生走光。" << endl;
 } else if (self_shutdown) {
  cout << " 自愿退出。" << endl;
 } else {
  cout << " 这条消息永远都不会显示。是不是很奇怪？" << endl;
 }
 cout << "老师最终总共教了 " << stud_total << " 个学生，赚了 " << m << "元……" << endl;
 cout << "[游戏结束]..." << endl;
 getch();
}

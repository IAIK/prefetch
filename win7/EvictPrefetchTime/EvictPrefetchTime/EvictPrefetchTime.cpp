// EvictPrefetchTime.cpp : Definiert den Einstiegspunkt für die Konsolenanwendung.
//

#include "stdafx.h"
#include <windows.h>
#include <intrin.h>
#include <assert.h>

#pragma region ioctldefs
#define	IOCTL_SBP_BASE					0xA013
#define	IOCTL_SBP_START	CTL_CODE(IOCTL_SBP_BASE, 0x805,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_SBP_STOP	CTL_CODE(IOCTL_SBP_BASE, 0x806,	METHOD_BUFFERED, FILE_ANY_ACCESS)

#define IOCTL_SBP_LOADCR4  CTL_CODE(IOCTL_SBP_BASE, 0x807,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_GETPHYSICALADDRESS  CTL_CODE(IOCTL_SBP_BASE, 0x808,	METHOD_BUFFERED, FILE_ANY_ACCESS)

#define IOCTL_SBP_GETMSR  CTL_CODE(IOCTL_SBP_BASE, 0x809,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_SETMSR  CTL_CODE(IOCTL_SBP_BASE, 0x80a,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_FLUSH  CTL_CODE(IOCTL_SBP_BASE, 0x80b,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_ACCESS_MEMORY  CTL_CODE(IOCTL_SBP_BASE, 0x80c,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_PREFETCH  CTL_CODE(IOCTL_SBP_BASE, 0x80d,	METHOD_BUFFERED, FILE_ANY_ACCESS)



typedef struct PHYSICAL_ADDRESS
{
	LARGE_INTEGER a;
}_PHYSICAL_ADDRESS,*PPHYSICAL_ADDRESS;
typedef struct _LOAD_CR4
{
	ULONG CR4;

}LOAD_CR4, *PLOAD_CR4;

typedef struct _GETPHYSICAL_ADDRESS
{

	PVOID				VirtualAddress;
	PHYSICAL_ADDRESS	Address;

}GETPHYSICAL_ADDRESS, *PGETPHYSICAL_ADDRESS;

typedef struct _GETSETMSR
{
	ULONG MSR;
	ULONGLONG Value;
}GETSETMSR, *PGETSETMSR;
#pragma endregion
__int64 TimeMemoryAccess(int * pointer)
{
	
	
	unsigned __int64 StartTime;
	unsigned __int64 EndTime;
	unsigned int  rdtscp = 0;
	int a;
	_mm_mfence();
	

	StartTime = __rdtscp(&rdtscp);
	_ReadBarrier();
	a = *pointer;
	_ReadBarrier();
	_mm_mfence();
	EndTime = __rdtscp(&rdtscp);;
	if (a)
	{
		return (EndTime - StartTime - 1);
	}
	return  EndTime - StartTime;
}

#pragma endregion




int q;
 
#pragma region Performance counters
#define MSR_WRITE 1		// use WDMSR
#define MSR_READ 2		// use RMMSR
#define MSR_STOP 0		// Script is ended i

typedef struct _MSR_SCRIPT
{
	DWORD Type;
	DWORD MSRRegister;
	ULONGLONG Param1; // lower 32bit
	DWORD Param2; // upper 32bit
} MSR_SCRIPT, *PMSR_SCRIPT;

class PerformanceCounting
{
public:
	PerformanceCounting()
	{
		m_hFile =  ::CreateFile(L"\\\\.\\mydriver_link", GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
	};

	void RunMSRScript(MSR_SCRIPT* Script, DWORD * Results);

	HANDLE m_hFile;
};


typedef struct _PMU_MSR
{
	//18.2 in Intel Software dev. Systems programming.
	ULONG	EventSelect : 8;	// Event selection
	ULONG	UnitMask : 8;	// Unit mask
	ULONG	USR : 1;	// privLevl
	ULONG	OS : 1;	// privLevl
	ULONG	E : 1;	//edge
	ULONG	PC : 1;
	ULONG	Interupt : 1;
	ULONG	Reseved : 1;	// I think this was assigned meaning on more modern processors than mine. You should check it out. If I recall correctly it's something to do with threading..
	ULONG	Env : 1;
	ULONG	Inv : 1;
	ULONG	CMask : 8;
}PMU_MSR, *PPMU_MSR;

MSR_SCRIPT msr_resetcount[] = {

	{ MSR_WRITE, 0xc1, -1000, 0x0, },        // ia32_pmc0: zero value (35-5)
	{ MSR_STOP, 0x00, 0x00,0x00 }
};

MSR_SCRIPT msr_start[] = {
	{ MSR_WRITE, 0x38f, 0x00, 0x00 },			// ia32_perf_global_ctrl: disable 4 PMCs & 3 FFCs
	{ MSR_WRITE, 0xc1, 0, 0x0, },        // ia32_pmc0: zero value (35-5)
	{ MSR_WRITE, 0xc2, -0, 0x00 },        // ia32_pmc1: zero value (35-5)
	{ MSR_WRITE, 0x186, 0x4101c2, 0x00 },		// ia32_perfevtsel1, LLC Misses		<--- check user mode code to figure out the values...
												//{ MSR_WRITE, 0x187, 0x0041010e, 0x00 },		// ia32_perfevtsel0, UOPS_ISSUED.ANY (19.22) (NO interupt..)
	{ MSR_WRITE, 0x187, 0x00430090, 0x00 },		// ia32_perfevtsel0, RET MISSES (19.22) (NO interupt..)
	{ MSR_READ, 0x186, 0x00,0x00 },             // ia32_pmc0: read value (35-5) // Check that I'm setting it correctly...
	{ MSR_READ, 0x187, 0x00,0x00 },
	{ MSR_READ, 0xc1, 0x00,0x00 },              // ia32_pmc0: read value (35-5)// Check that I'm setting it correctly...
	{ MSR_READ, 0xc2, 0x00,0x00 },
	{ MSR_WRITE, 0x38f, 0x03,0x00 },			// ia32_perf_global_ctrl: enable 2 PMCs 
	{ MSR_STOP, 0x00, 0x00,0x00 }
};

MSR_SCRIPT msr_stop[] = {
	{ MSR_WRITE, 0x38f, 0x00, 0x00 },			// ia32_perf_global_ctrl: disable performance counterss
	{ MSR_READ, 0xc1, 0x00,0x00 },              // ia32_pmc0: read value (35-5)
	{ MSR_READ, 0xc2, 0x00,0x00 },              // ia32_pmc1: read value (35-5)
	{ MSR_STOP, 0x00, 0x00 ,0x00 }
};

void PerformanceCounting::RunMSRScript(MSR_SCRIPT* Script, DWORD * Results)
{
	int i;
	int result;
	DWORD dwReturn;
	DWORD Param1;
	DWORD Param2;

	i = 0;
	
	
	GETSETMSR a;

	
	while (1)
	{
		if (Script[i].Type == MSR_STOP)
		{
			break;
		}
		else if (Script[i].Type == MSR_WRITE)
		{
			a.MSR = Script[i].MSRRegister;
			a.Value = Script[i].Param1;
			DeviceIoControl(m_hFile, IOCTL_SBP_SETMSR, (VOID*)&a, sizeof(GETSETMSR), (VOID*)&a, sizeof(GETSETMSR), &dwReturn, NULL);
		}
		else if (Script[i].Type == MSR_READ)
		{
			a.MSR = Script[i].MSRRegister;
			a.Value = Script[i].Param1;
			DeviceIoControl(m_hFile, IOCTL_SBP_GETMSR, (VOID*)&a, sizeof(GETSETMSR), (VOID*)&a, sizeof(GETSETMSR), &dwReturn, NULL);

		}
		i++;
	}
	return;
}


#pragma endregion
bool GetPhys()
{
	DWORD dwReturn;
	
	GETSETMSR a;
	a.MSR = 0xC0000082;
	GETSETMSR b;
	//b.MSR = 0x0C0000102;

	GETPHYSICAL_ADDRESS c;
	GETPHYSICAL_ADDRESS d;
	

	HANDLE hFile = ::CreateFile(L"\\\\.\\mydriver_link", GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
	::DeviceIoControl(hFile, IOCTL_SBP_GETMSR, (VOID*)&a, sizeof(GETSETMSR), (VOID*)&a, sizeof(GETSETMSR), &dwReturn, NULL);
	c.VirtualAddress = (PVOID) &q;
	//::DeviceIoControl(hFile, IOCTL_SBP_GETMSR, (VOID*)&b, sizeof(GETSETMSR), (VOID*)&b, sizeof(GETSETMSR), &dwReturn, NULL);
	d.VirtualAddress = (PVOID)0xfffff80002ff5000;
	::DeviceIoControl(hFile, IOCTL_SBP_GETPHYSICALADDRESS, (VOID*)&c, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&c, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);

	::DeviceIoControl(hFile, IOCTL_SBP_GETPHYSICALADDRESS, (VOID*)&d, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&d, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);

	return true;
}



bool FlushKernelModeAddress(HANDLE hDriver, PVOID Address)
{
	DWORD dwReturn;
	GETPHYSICAL_ADDRESS a;
	a.VirtualAddress = Address;
	return ::DeviceIoControl(hDriver, IOCTL_SBP_FLUSH, (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);
}
bool PrefetchKernelModeAddress(HANDLE hDriver, PVOID Address)
{
	DWORD dwReturn;
	GETPHYSICAL_ADDRESS a;
	a.VirtualAddress = Address;
	return ::DeviceIoControl(hDriver, IOCTL_SBP_PREFETCH, (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);
}


PVOID GetKernelAddress(HANDLE hDriver)
{
	GETPHYSICAL_ADDRESS a;
	DWORD dwReturn;
	a.VirtualAddress = 0;
	::DeviceIoControl(hDriver, IOCTL_SBP_ACCESS_MEMORY, (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);

	return a.VirtualAddress;

}

bool AccessKernelAddress(HANDLE hDriver, PVOID Address)
{
	GETPHYSICAL_ADDRESS a;
	DWORD dwReturn;
	a.VirtualAddress = Address;
	return ::DeviceIoControl(hDriver, IOCTL_SBP_ACCESS_MEMORY, (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), (VOID*)&a, sizeof(GETPHYSICAL_ADDRESS), &dwReturn, NULL);

}
#define EVICT 	for (size_t q = 0; q < 1024 * 1024 * 4; q += 32)\
					*(BigBuffer + q);
int main()
{
	unsigned __int64 Histogram[5000] = { 0 };
	unsigned __int64 Histogram2[5000] = { 0 };
	unsigned __int64 Histogram3[5000] = { 0 };
	unsigned __int64 StartTime;
	unsigned __int64 EndTime;
	unsigned __int64 UncachedTime;

	unsigned __int64 CachedTime;
	unsigned int rdtscp;
	printf("Q: %d\n", (int*) TimeMemoryAccess(&q));
	DWORD dwReturn;
	GETPHYSICAL_ADDRESS a;
	a.VirtualAddress = (PVOID) main;
	
	int Count = 0;
	int Start = GetTickCount();
	HANDLE hFile = ::CreateFile(L"\\\\.\\mydriver_link", GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		printf("Driver nBot present...");
		return 0;
	}
	PVOID AttackAddress = GetKernelAddress(hFile);
	
	size_t* BigBuffer = (size_t*) VirtualAlloc(NULL, 1024 * 1024 * 4, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	//AttackAddress = (PVOID)qqq;

	int res[4];

	printf("SizeT: %d\n", sizeof(size_t));
	
	for (int qq = -20; qq < 50; qq++)
	{
	
		UncachedTime = CachedTime = 1000;
		
		const char * PrefAddr = ((const char*)AttackAddress) + (qq * 4096);
		//prefetch right address
		for (int i = 0; i < 100; i++)
		{
			
			Sleep(10);
			//
			//EVICT;
			
			AccessKernelAddress(hFile, AttackAddress);
			AccessKernelAddress(hFile, AttackAddress);
			AccessKernelAddress(hFile, AttackAddress);
			
			//FlushKernelModeAddress(hFile, AttackAddress);
			//GetKernelAddress(hFile);
			//GetKernelAddress(hFile);
			//GetKernelAddress(hFile);
			//for (int i = 0; i < 3; ++i)
			{
				//SwitchToThread();
				//_mm_prefetch((const char*)AttackAddress+ (qq*4096), _MM_HINT_NTA);
				//_mm_prefetch((const char*)AttackAddress + (qq * 4096), _MM_HINT_T2);
				//GetKernelAddress(hFile);
				//
			}
			
			_mm_mfence();
			__cpuid(res, 0);
			_mm_mfence();
			StartTime = __rdtsc();
			_mm_prefetch(PrefAddr, _MM_HINT_NTA);
			_mm_prefetch(PrefAddr, _MM_HINT_T2);
			_mm_mfence();
			EndTime = __rdtscp(&rdtscp);
			__cpuid(res, 0);


			CachedTime = min(CachedTime, EndTime - StartTime);
			Histogram[min(5000-1, EndTime - StartTime)]++;
		}
		
	
		
			printf("%d %I64x %d\n", qq, PrefAddr,CachedTime);
		if (CachedTime < UncachedTime)
		{
			Count++;
		}


		
	}


	int Sum = 0;
	int Sum2 = 0;
	printf("COunt: %d\n", Count);
	printf("Time \t Userprefetch \t kernel prefet\n");
	for (int i = 0; i < 600; i++)
	{
		Sum += Histogram[i];
		Sum2 += Histogram2[i];
		if (Histogram[i] + Histogram2[i] > 0)
		{
			printf("%.4d: %.6d  %.6d  %.6d -- %.4d   %.4d\n",i, Histogram[i], Histogram2[i], Histogram3[i], Sum*100/100000,Sum2 * 100 / 100000);
		}
	}
		int End = GetTickCount();

	int Size = End - Start; 


	DWORD dwJunk;

	::CloseHandle(hFile);



    return 0;
}


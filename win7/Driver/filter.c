#include <ntddk.h>
#include <filter.h>

extern	NTKERNELAPI	void		KeSetAffinityThread(PKTHREAD i,	ULONG i2);
//extern NTSYSAPI CCHAR KeNumberProcessors;

#pragma region strucdefs
typedef struct _LOAD_CR4
{
	ULONG CR4;

}LOAD_CR4,*PLOAD_CR4;

typedef struct _GETSETMSR
{
	ULONG MSR;
	ULONGLONG Value;
}GETSETMSR, *PGETSETMSR;

typedef struct _GETPHYSICAL_ADDRESS
{

	PVOID		VirtualAddress;
	PHYSICAL_ADDRESS Address;

}GETPHYSICAL_ADDRESS, *PGETPHYSICAL_ADDRESS;


typedef struct _STACK_VERIFICATION_DATA	
{
	//
	// I don't really use this structure for anything, it's a legacy from the one driver I had sources for. It used to contain informations about hooks in all drivers
	// in the stack above and below a device object used in a filter driver of mine.
	//
	ULONG	EverythingOk;
	ULONG	Register1;
	ULONG	Register2;
	ULONG	Count1;
	ULONG	Count2;

} STACK_VERIFICATION_DATA, *PSTACK_VERIFICATION_DATA;

#pragma endregion

NTSTATUS DriverEntry(PDRIVER_OBJECT pDriverObject, PUNICODE_STRING pRegistryPath);
DRIVER_UNLOAD Unload;
NTSTATUS DriverCreate(PDEVICE_OBJECT DeviceObject, PIRP Irp);
NTSTATUS DriverClose(PDEVICE_OBJECT DeviceObject, PIRP Irp);
NTSTATUS DriverUnSupportedFunction(PDEVICE_OBJECT DeviceObject, PIRP Irp);
NTSTATUS	DispatchIo(	 IN PDEVICE_OBJECT  DeviceObject, IN PIRP Irp);



// See pageable code
#pragma alloc_text(INIT, DriverEntry)
#pragma alloc_text(PAGE, Unload)
#pragma alloc_text(PAGE, DriverCreate) 
#pragma alloc_text(PAGE, DriverClose) 
#pragma alloc_text(PAGE, DriverUnSupportedFunction)
#pragma alloc_text(PAGE, DispatchIo)

                            // set => user mode



DWORD SetMSR( DWORD MsrRegister, DWORD * Value1, DWORD * Value2);
static	int	NumberOfProcessors()
{

	return	(int)KeNumberProcessors;
}



//////////////////////////////////////////////////////////////////////////

__declspec(align(64)) PVOID UnusedInt = (PVOID)0xBADF00D;
__declspec(align(64)) int UnusedInt2 = 0;

INT_VECTOR		g_OldVector = {0};
int				g_Indicator[5] = {0}; // use this to get context information about where the interupt happend...
int				g_InteruptCount =0;
DWORD			Chain =0;

DWORD Minus1 = -1;
DWORD Minus0 = 0;
DWORD EipCounter=0;
DWORD EipVector[20];
/*
void __declspec(naked) 
	LogStuff()
{

	if (g_InteruptCount < 20)
	{
		EipVector[g_InteruptCount] =g_Indicator[2];
		//SetMSR(0xC1,&Minus1,&Minus0);
	}
}*/






			    	


DWORD GetMSR( DWORD MsrRegister, DWORD * Value1, DWORD * Value2)
{	 
	/*
	__asm 
	{
		pushad
		mov ecx, [MsrRegister]
				
		rdmsr
		
		mov ebx, [Value1]
		mov [ebx], eax
		
		mov ebx, [Value2]
		mov [ebx], edx

		popad
		 
	}
	*/
	return 0;
}

DWORD SetMSR( DWORD MsrRegister, DWORD * Value1, DWORD * Value2)
{
	/*
		__asm 
	{
		pushad
		mov ecx, [MsrRegister]
		mov eax, [Value1]		
		mov eax,[eax]
		mov edx, [Value2]
		mov edx, [edx]
		
		wrmsr

		popad
		 
	}
	*/
	return 0;
}

#define MSR_WRITE 1		// use WDMSR
#define MSR_READ 2		// use RMMSR
#define MSR_STOP 0		// Script is ended here
typedef struct _MSR_SCRIPT
{
	DWORD Type;
	DWORD MSRRegister;
	DWORD Param1; // lower 32bit
	DWORD Param2; // upper 32bit
} MSR_SCRIPT, *PMSR_SCRIPT;
		

	MSR_SCRIPT msr_resetcount[] = {
	
	{ MSR_WRITE, 0xc1, -1000, 0x0, },        // ia32_pmc0: zero value (35-5)
	{ MSR_STOP, 0x00, 0x00,0x00 }
	};
		MSR_SCRIPT msr_start[] = {
        { MSR_WRITE, 0x38f, 0x00, 0x00 },			// ia32_perf_global_ctrl: disable 4 PMCs & 3 FFCs
        { MSR_WRITE, 0xc1, -1000, 0x0, },        // ia32_pmc0: zero value (35-5)
        { MSR_WRITE, 0xc2, -1000, 0x00 },        // ia32_pmc1: zero value (35-5)
       
	    { MSR_WRITE, 0x186, 0x0051412e, 0x00 },		// ia32_perfevtsel1, LLC Misses		<--- check user mode code to figure out the values...
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
        { MSR_STOP, 0x00, 0x00 ,0x00}
    };


void RunMSRScript(MSR_SCRIPT* Script, DWORD * Results)
{
	int i;
	int result;
	DWORD Param1;
	DWORD Param2;
	
	i = 0;
	
	while (1)
	{
		if (msr_start[i].Type == MSR_STOP)
		{
			break;
		}
		else if (Script[i].Type == MSR_WRITE)
		{
			SetMSR(Script[i].MSRRegister,&Script[i].Param1,&Script[i].Param1);
		}
		else if (Script[i].Type == MSR_READ)
		{
			GetMSR(Script[i].MSRRegister, &Param1, &Param2);
			DbgPrint("MSR Fetched %x %x from %x", Param1, Param2,Script[i].MSRRegister);
			
		}
		i++;
	}
	return;
}			    	

int DoMSRs(int start)
{
	DWORD Values[8];
//	for( cpu = 0; cpu <	NumberOfProcessors(); ++cpu	)	// You should do it on all CPU's. I don't here (to minimize Dbg output)
	{

		PKTHREAD	pThread	= KeGetCurrentThread();
		if (start)
		{
			RunMSRScript(msr_start, &Values[0]);
		}
		else
		{

			RunMSRScript(msr_stop, &Values[0]);
		}
		

	}
	return 0;
}

		
NTSTATUS DriverCreate(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    // this function has been marked for paging above (with pragma statement)
    // this macro checks if the code really runs as pageable code.
    PAGED_CODE();

    //DbgPrint("DriverCreate Called \r\n");

    // This call is very important for memory management. You can get random bluescreens
    // if you return from an IRP handling function without calling this
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    return Irp->IoStatus.Status;
}

NTSTATUS DriverClose(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    // this function has been marked for paging above (with pragma statement)
    // this macro checks if the code really runs as pageable code.
    PAGED_CODE();

    DbgPrint("DriverClose Called \r\n");

    // This call is very important for memory management. You can get random bluescreens
    // if you return from an IRP handling function without calling this
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    return Irp->IoStatus.Status;
}

#pragma region timememoryaccess

__int64 TimeMemoryAccess(int * pointer)
{
	int junk;
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
#pragma region DriverUnSupportedFunction

NTSTATUS DriverUnSupportedFunction(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    // this function has been marked for paging above (with pragma statement)
    // this macro checks if the code really runs as pageable code.
    PAGED_CODE();

    //DbgPrint("DriverUnSupportedFunction Called \r\n");

    // We need to set this variable to the same value as what is returned from this function
    Irp->IoStatus.Status = STATUS_NOT_SUPPORTED;

    // This call is very important for memory management. You can get random bluescreens
    // if you return from an IRP handling function without calling this
    IoCompleteRequest(Irp, IO_NO_INCREMENT);

    return Irp->IoStatus.Status;
}
#pragma endregion

NTSTATUS
DispatchIo(
				 IN PDEVICE_OBJECT    DeviceObject,
				 IN PIRP              Irp
				 )
				 
{
	PIO_STACK_LOCATION  irpStack;
	NTSTATUS            status;
	int i;
	
	ULONG				SizeOfInformation;
	ULONG				BufferSize;
	PSTACK_VERIFICATION_DATA pIoctlBuffer;
	STACK_VERIFICATION_DATA ReturnBuffer;
	PLOAD_CR4				pCr4BufferIn;
	PLOAD_CR4				pCr4BufferOut;
	PGETPHYSICAL_ADDRESS		pPhysicalAddressIn;
	PGETPHYSICAL_ADDRESS		pPhysicalAddressOut;
	PGETSETMSR					pGetSetMsrIn;
	PGETSETMSR					pGetSetMsrOut;
	
	

	PAGED_CODE();


	
	
		status = STATUS_SUCCESS;
		Irp->IoStatus.Information = 0;
		irpStack = IoGetCurrentIrpStackLocation (Irp);

		switch (irpStack->MajorFunction) 
		{

			case  IRP_MJ_DEVICE_CONTROL:
								
				DbgPrint("DeviceIoControl %d\n",irpStack->Parameters.DeviceIoControl.IoControlCode);
				switch (irpStack->Parameters.DeviceIoControl.IoControlCode) 
				{
					//
					//case IOCTL_CUSTOM_CODE: 
					//
				case IOCTL_SBP_STOP:
						BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					pIoctlBuffer = Irp->AssociatedIrp.SystemBuffer;

					if	((pIoctlBuffer != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength>=sizeof(STACK_VERIFICATION_DATA))
						|| ( BufferSize>=sizeof(STACK_VERIFICATION_DATA)))

					{
						DWORD Values[7];
						 DbgPrint("Interupt left this for us Count: %d : %x %x eip(%x) %x %x\n",g_InteruptCount, g_Indicator[0],g_Indicator[1],g_Indicator[2],g_Indicator[3],g_Indicator[4] );
						
						 for (i=0;i<20;i++)
						 {

							 DbgPrint("Interupt EIP: %x\n",EipVector[i]);
						 }
						 //RunMSRScript(msr_stop, &Values[0]);
						 DoMSRs(0);
						 //HookInterupt(Chain);
						 //InstallInteruptHandlerAndSetMSR();
						ReturnBuffer.EverythingOk  = 1;
						memcpy( Irp->AssociatedIrp.SystemBuffer, &ReturnBuffer,sizeof(STACK_VERIFICATION_DATA));
						Irp->IoStatus.Information = sizeof(STACK_VERIFICATION_DATA);
					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}
					


					break;

				case IOCTL_SBP_ACCESS_MEMORY:
					pPhysicalAddressIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if ((pPhysicalAddressIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength >= sizeof(GETPHYSICAL_ADDRESS))
						|| (BufferSize >= sizeof(GETPHYSICAL_ADDRESS)))

					{
						
						pPhysicalAddressOut = (PGETPHYSICAL_ADDRESS)Irp->AssociatedIrp.SystemBuffer;
						if (pPhysicalAddressIn->VirtualAddress)
						{
							PVOID * a = (PVOID *) pPhysicalAddressIn->VirtualAddress;
							pPhysicalAddressOut->VirtualAddress = (PVOID) *a;
						}
						else
						{
							pPhysicalAddressOut->VirtualAddress = (PVOID) &UnusedInt;
							DbgPrint("sdfsdf: %d", pPhysicalAddressIn->VirtualAddress);
						}
						
						DbgPrint("pPhysicalAddressIn: %d", pPhysicalAddressIn->VirtualAddress);
						Irp->IoStatus.Information = sizeof(GETPHYSICAL_ADDRESS);

					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}

					break;
				case IOCTL_SBP_PREFETCH:
					pPhysicalAddressIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if ((pPhysicalAddressIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength >= sizeof(GETPHYSICAL_ADDRESS))
						|| (BufferSize >= sizeof(GETPHYSICAL_ADDRESS)))

					{
						_mm_prefetch((const char*)pPhysicalAddressIn->VirtualAddress, _MM_HINT_NTA);
						_mm_prefetch((const char*)pPhysicalAddressIn->VirtualAddress, _MM_HINT_T2);
						Irp->IoStatus.Information = sizeof(GETPHYSICAL_ADDRESS);

					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}

					break;
				case IOCTL_SBP_FLUSH:
					pPhysicalAddressIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if ((pPhysicalAddressIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength >= sizeof(GETPHYSICAL_ADDRESS))
						|| (BufferSize >= sizeof(GETPHYSICAL_ADDRESS)))

					{
						_mm_clflush(pPhysicalAddressIn->VirtualAddress);
						Irp->IoStatus.Information = sizeof(GETPHYSICAL_ADDRESS);

					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}

					break;
				case IOCTL_SBP_GETMSR:
				case IOCTL_SBP_SETMSR:
					pGetSetMsrIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if ((pGetSetMsrIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength >= sizeof(GETSETMSR))
						|| (BufferSize >= sizeof(GETSETMSR)))

					{
						if (irpStack->Parameters.DeviceIoControl.IoControlCode == IOCTL_SBP_GETMSR)
						{
							pGetSetMsrOut = (PGETSETMSR)Irp->AssociatedIrp.SystemBuffer;
							pGetSetMsrOut->Value = __readmsr(pGetSetMsrIn->MSR);
						}
						else
						{
							__writemsr(pGetSetMsrIn->MSR, pGetSetMsrIn->Value);
						}
						
						Irp->IoStatus.Information = sizeof(GETSETMSR);

					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}
					break;

				case IOCTL_SBP_GETPHYSICALADDRESS:
					pPhysicalAddressIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if ((pPhysicalAddressIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength >= sizeof(GETPHYSICAL_ADDRESS))
						|| (BufferSize >= sizeof(GETPHYSICAL_ADDRESS)))

					{
						
						DbgPrint("In: %I64x %I64x %I64x\n",  pPhysicalAddressIn->VirtualAddress, &g_InteruptCount, MmGetPhysicalAddress(&g_InteruptCount));
						pPhysicalAddressOut = (PGETPHYSICAL_ADDRESS)Irp->AssociatedIrp.SystemBuffer;
						pPhysicalAddressOut->VirtualAddress = (PVOID)pPhysicalAddressIn->VirtualAddress;
						pPhysicalAddressOut->Address = MmGetPhysicalAddress(pPhysicalAddressIn->VirtualAddress);
						Irp->IoStatus.Information = sizeof(GETPHYSICAL_ADDRESS);
						
					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}

					break;
				case IOCTL_SBP_LOADCR4:
					pCr4BufferIn = Irp->AssociatedIrp.SystemBuffer;
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					if	((pCr4BufferIn != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength>=sizeof(LOAD_CR4))
						|| ( BufferSize>=sizeof(LOAD_CR4)))

					{
						ULONG tmp;
						if (pCr4BufferIn->CR4)
						{
							tmp = (DWORD) pCr4BufferIn->CR4;
							/*
							__asm 
							{ 
								push eax
								mov eax, tmp
								_emit 0x0f	// mov cr4, eax 	3:  0f 22 e0                mov    cr4,eax
								_emit 0x22
								_emit 0xe0
								pop eax
							}

							
							
							__asm pop eax;
							*/
						}
						/*
						__asm push eax
					
						__asm _emit 0x0f//0:  0f 20 e0                mov    eax,cr4
						__asm _emit 0x20
						__asm _emit 0xe0
						__asm mov tmp, eax
						__asm pop eax;
						*/
						pCr4BufferOut = (PLOAD_CR4)  Irp->AssociatedIrp.SystemBuffer;
						pCr4BufferOut->CR4 = tmp;
						Irp->IoStatus.Information = sizeof(LOAD_CR4);
					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
					}

					break;
				case IOCTL_SBP_START:
					BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
					pIoctlBuffer = Irp->AssociatedIrp.SystemBuffer;

					if	((pIoctlBuffer != NULL)
						|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength>=sizeof(STACK_VERIFICATION_DATA))
						|| ( BufferSize>=sizeof(STACK_VERIFICATION_DATA)))

					{
						for (i=0;i<20;i++)
						{

							EipVector[i]=0;
						}
						EipCounter = 0;
						 DbgPrint("IOCTL_SBP_START request...\n");
						// HookInterupt((DWORD)NewStaticInt1Handler);

						 if (pIoctlBuffer->Register1)
						 {
							 msr_start[3].Param1 = pIoctlBuffer->Register1;
						 }

						 if (pIoctlBuffer->Count1)
						 {
							 msr_start[1].Param1 = pIoctlBuffer->Count1;
						 }

						 if (pIoctlBuffer->Register2)
						 {
							 msr_start[4].Param1 = pIoctlBuffer->Register2;
						 }

						 if (pIoctlBuffer->Count2)
						 {
							 msr_start[2].Param1 = pIoctlBuffer->Count2;
						 }

						 g_InteruptCount= g_Indicator[0]=g_Indicator[1]=g_Indicator[2]=g_Indicator[3]=g_Indicator[4]=0;

						DoMSRs(1);
						ReturnBuffer.EverythingOk  = 1;
						memcpy( Irp->AssociatedIrp.SystemBuffer, &ReturnBuffer,sizeof(STACK_VERIFICATION_DATA));
						Irp->IoStatus.Information = sizeof(STACK_VERIFICATION_DATA);
					}
					else
					{
						status = STATUS_INVALID_PARAMETER;
						_mm_clflush(&g_InteruptCount);
					}
					break;
			
			
			
		
			default:
				status = STATUS_INVALID_PARAMETER;
				break;
				} // inner switch
			default:
				break;
		}// Outter switch
	
	
	Irp->IoStatus.Status = status;
	IoCompleteRequest (Irp, IO_NO_INCREMENT);
	return status;
}



NTSTATUS DriverEntry(PDRIVER_OBJECT pDriverObject, PUNICODE_STRING pRegistryPath)
{
    int uiIndex = 0;
    NTSTATUS status = STATUS_SUCCESS;
    
    UNICODE_STRING usDriverName, usDosDeviceName;
    PDEVICE_OBJECT pDeviceObject = NULL;
    
    RtlInitUnicodeString(&usDriverName, L"\\Device\\mydriver");
    RtlInitUnicodeString(&usDosDeviceName, L"\\DosDevices\\mydriver_link");
    
    status = IoCreateDevice(pDriverObject, 0,
                            &usDriverName,
                            FILE_DEVICE_UNKNOWN, FILE_DEVICE_SECURE_OPEN,
                            FALSE, &pDeviceObject);
                            
    if(status != STATUS_SUCCESS)
    {
        DbgPrint("mydriver IoCreateDevice failed\n");
        return status;
    }

    for(uiIndex = 0; uiIndex < IRP_MJ_MAXIMUM_FUNCTION; uiIndex++)
    {
        pDriverObject->MajorFunction[uiIndex] = DriverUnSupportedFunction;
    }
    
    pDriverObject->MajorFunction[IRP_MJ_CLOSE]             = DriverClose;
    pDriverObject->MajorFunction[IRP_MJ_CREATE]            = DriverCreate;
		pDriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] 	 = DispatchIo;
    IoDeleteSymbolicLink(&usDosDeviceName);

    status = IoCreateSymbolicLink(&usDosDeviceName, &usDriverName);
    if(status != STATUS_SUCCESS)
    {
        DbgPrint("mydriver IoCreateSymbolicLink failed\n");
        IoDeleteDevice(pDeviceObject);
        return status;
    }
   
    pDriverObject->DriverUnload = Unload;
    
    DbgPrint("mydriver loaded successfully v 10\n");
    return STATUS_SUCCESS;
}

VOID Unload(PDRIVER_OBJECT DriverObject)
{
    NTSTATUS status = STATUS_SUCCESS;

    UNICODE_STRING usDosDeviceName;

	//
	// Cleaning up and unhooking here would be a nice idea!
	//

    // this function has been marked for paging above (with pragma statement)
    // this macro checks if the code really runs as pageable code.
    PAGED_CODE();

    RtlInitUnicodeString(&usDosDeviceName, L"\\DosDevices\\mydriver_link");

    status = IoDeleteSymbolicLink(&usDosDeviceName);
    if (status != STATUS_SUCCESS)
    {
        DbgPrint("mydriver IoDeleteSymbolicLink failed\n");
    }

    IoDeleteDevice(DriverObject->DeviceObject);
    
    DbgPrint("mydriver unloaded successfully\n");
}

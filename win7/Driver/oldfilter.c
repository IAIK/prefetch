/*++


--*/

#include "filter.h"
#include <ntddscsi.h>
#include <scsi.h>



#if defined(_WIN64)
#else
#define PSCSI_PASS_THROUGH_DIRECT32 PSCSI_PASS_THROUGH_DIRECT
#define SCSI_PASS_THROUGH_DIRECT32 PSCSI_PASS_THROUGH_DIRECT
#define IoIs32bitProcess(_X_) TRUE

#endif
#ifdef ALLOC_PRAGMA

#pragma alloc_text (INIT, DriverEntry)
#pragma alloc_text (PAGE, FilterAddDevice)
#pragma alloc_text (PAGE, FilterDispatchPnp)
#pragma alloc_text (PAGE, FilterUnload)
#endif



#if 0
//__declspec(dllimport) void VirtualizerStart(void);
//__declspec(dllimport) void VirtualizerEnd(void);
__declspec(dllimport) void __stdcall VMProtectBegin(char *);
__declspec(dllimport) void __stdcall VMProtectEnd(void);



#define VirtualizerStartA VMProtectBegin(NULL) //VirtualizerStart();
#define VirtualizerEndA  VMProtectEnd() // VirtualizerEnd();
#else

#define VirtualizerStartA 
#define VirtualizerEndA	

#endif


#define B2N_32(x) \
 x = ((((x) & 0xff000000) >> 24) | \
      (((x) & 0x00ff0000) >>  8) | \
      (((x) & 0x0000ff00) <<  8) | \
      (((x) & 0x000000ff) << 24))

int Evil =0;
ULONG ByteSwap32(ULONG k)
{
	ULONG j = k;
	k = B2N_32(j);
	return k;
};



PUCHAR DoScsiCommand(PUCHAR Cdb, PDEVICE_OBJECT DeviceObject, PDEVICE_EXTENSION deviceExtension, 
											PUCHAR DataBuffer, ULONG TransferLength, ULONG IsSonic, ULONG bCanSwapBuffer);



void EncryptProjectId(PUCHAR pProjectId, PUCHAR Output, PUCHAR pKey);

ULONG	g_Disabled = 0;					// passes through everything
ULONG	g_MaxBurns = 0;					// Number of licenses
ULONG	g_WritesInProgress;				// Number of burns simultaniously in progress
ULONG	g_UsedSinceLastTime = 0;		// Number of licenses used since last poll
ULONG	g_NoLicenseBurns=0;				// Number of burns w/o license since last poll.

CHAR MsgBuffer[512];
typedef unsigned long DWORD;

void DecreasewritesInProgress(PDEVICE_EXTENSION           deviceExtension)
{
	__asm
	{
		mov eax, deviceExtension
		sidt [eax]
	}
	if (deviceExtension->WriteInProgress)
	{
		ADebugPrint(("Decreasing WritesInProgress %d\n",g_WritesInProgress));	
		g_WritesInProgress--;
		deviceExtension->WriteInProgress = 0;
	}
	return;
}


void DecreaseLicense(PDEVICE_EXTENSION           deviceExtension)
{
	VirtualizerStartA;
	

	//
	// Delete all writes in progress
	//
	DecreasewritesInProgress(deviceExtension);
		
	ADebugPrint(("%x Last sector written: %d %x\n",deviceExtension, deviceExtension->AssumedNextWriteCommand,deviceExtension->DecreaseLicense));
	if (( (deviceExtension->DecreaseLicense != 0) && ( deviceExtension->AssumedNextWriteCommand >= (deviceExtension->DiscSize -2000)) )
			|| (deviceExtension->DecreaseLicense != 0) && ( deviceExtension->AssumedNextWriteCommand >= deviceExtension->SectorForPFI) && (deviceExtension->SectorForPFI > 0 ) )
	{
		g_MaxBurns--;
		g_UsedSinceLastTime++;
		deviceExtension->EndSector = 0;
		deviceExtension->AssumedNextWriteCommand = 0xBADF00D;
		deviceExtension->DecreaseLicense = 0;
		
		ADebugPrint(("%x Decreased license",deviceExtension)); 
	}
	else
	{
		
		ADebugPrint(("%x End of disc no license decrease",deviceExtension));
	}

	VirtualizerEndA;
	
	return;
}


//
// Define IOCTL_INTERFACE in your sources file if  you want your
// app to have private interaction with the filter driver. Read KB Q262305
// for more information.
//
#ifdef IOCTL_INTERFACE
/*
#ifdef ALLOC_PRAGMA
#pragma alloc_text (PAGE, FilterCreateControlObject)
#pragma alloc_text (PAGE, FilterDeleteControlObject)
#pragma alloc_text (PAGE, FilterDispatchIo)
#endif
*/
FAST_MUTEX ControlMutex;
ULONG InstanceCount = 0;
PDEVICE_OBJECT ControlDeviceObject;

#endif


#define SET_FLAG(Flags,SingleFlag) { \
	(Flags) |= (SingleFlag);        \
}

#define TEST_FLAG(F,SF) (    \
	(((F) & (SF)) != 0) \
	)
char  DeviceObjectToDriveLetter(PDEVICE_OBJECT pDevObj);


typedef struct _MyStackLocation
{
	__int64		ScsiPointer;
	__int64		DeviceIoPointer;
	__int64		DriverBase;
	char		DriverName[265];
}MyStackLocation, *PMyStackLocation;

MyStackLocation g_LastStackLocations[10];		// ANders: actually you should do some real synchroniatzion on these two variables!
int				g_NoStackLocs = -1;





PDEVICE_OBJECT IoGetAttachedDevice(PDEVICE_OBJECT);
NTSTATUS
SecureBurnGetTargetDevicePdo(
							 IN PDEVICE_OBJECT DeviceObject,
							 OUT PDEVICE_OBJECT *PhysicalDeviceObject
							 )
							 /*++

							 Routine Description:

							 This builds and send a pnp irp to get the PDO a device.

							 Arguments:

							 DeviceObject - This is the top of the device in the device stack 
							 the irp is to be sent to.

							 PhysicalDeviceObject - Address where the PDO pointer is returned

							 Return Value:

							 NT status code
							 --*/
{

	KEVENT                  event;
	NTSTATUS                status;
	PIRP                    irp;
	IO_STATUS_BLOCK         ioStatusBlock;
	PIO_STACK_LOCATION      irpStack;
	PDEVICE_RELATIONS       deviceRelations;
	char					filename[] = "\\DosDevices\\CdRom0";
	PDEVICE_OBJECT			TopDevice = DeviceObject;
	PFILE_OBJECT			fileObject;
	STRING					ntNameString;
	UNICODE_STRING			ntUnicodeString;
	PAGED_CODE();

	/*
	RtlInitString(&ntNameString, (char*) filename);

	if (!NT_SUCCESS(RtlAnsiStringToUnicodeString(&ntUnicodeString, &ntNameString, TRUE)))
	{
	//
	// This should never be the case
	//
	return STATUS_UNSUCCESSFUL;
	}



	status = IoGetDeviceObjectPointer(&ntUnicodeString,
	FILE_READ_ATTRIBUTES,
	&fileObject,
	&TopDevice);

	if (!NT_SUCCESS(status))
	{
	return status;
	}
	ObDereferenceObject(fileObject);
	*/
	KeInitializeEvent( &event, NotificationEvent, FALSE );

	irp = IoBuildSynchronousFsdRequest( IRP_MJ_PNP,
		TopDevice,
		NULL,
		0,
		NULL,
		&event,
		&ioStatusBlock );
	if (irp == NULL) {
		status = STATUS_INSUFFICIENT_RESOURCES;
		goto End;
	}

	irpStack = IoGetNextIrpStackLocation( irp );
	irpStack->MinorFunction = IRP_MN_QUERY_DEVICE_RELATIONS;
	irpStack->Parameters.QueryDeviceRelations.Type = TargetDeviceRelation;

	//
	// Initialize the status to error in case the bus driver decides not to
	// set it correctly.
	//

	irp->IoStatus.Status = STATUS_NOT_SUPPORTED ;


	status = IoCallDriver( TopDevice, irp );

	if (status == STATUS_PENDING) {

		KeWaitForSingleObject( &event, Executive, KernelMode, FALSE, NULL );
		status = ioStatusBlock.Status;
	}

	if (NT_SUCCESS( status)) {
		deviceRelations = (PDEVICE_RELATIONS)ioStatusBlock.Information;
		ASSERT(deviceRelations);
		//
		// You must dereference the PDO when it's no longer
		// required.
		//
		*PhysicalDeviceObject = deviceRelations->Objects[0];
		ExFreePool(deviceRelations);        
	}

End:
	return status;
}


NTSYSAPI
NTSTATUS
__stdcall
ZwQuerySystemInformation(
						 IN SYSTEM_INFORMATION_CLASS SystemInformationClass,
						 OUT PVOID SystemInformation,
						 IN ULONG SystemInformationLength,
						 OUT PULONG ReturnLength OPTIONAL
						 );


NTSTATUS
EnableDisableDriver(PUCHAR Buffer, ULONG BufferSize, int * BytesReturned)
{
	ULONG *pTmp = (ULONG*) Buffer;
	if ( BufferSize != 4)
	{
		return STATUS_INVALID_PARAMETER;
	}

	*BytesReturned =4;

	g_Disabled = pTmp[0];
	pTmp[0] = 5;
	return STATUS_SUCCESS;	
}
PDRIVER_OBJECT  g_DriverObject;
NTSTATUS AttachToPfc(IN PDRIVER_OBJECT  DriverObject)
{
	NTSTATUS nStatus;
	PFILE_OBJECT pFileObject = NULL;
	PDEVICE_OBJECT deviceObject = NULL;
	PDEVICE_OBJECT pDeviceObject = NULL;
	UNICODE_STRING NtDeviceName;
	PDEVICE_EXTENSION       deviceExtension;
	RtlInitUnicodeString ( &NtDeviceName, L"\\Device\\Paspi0" );
	
	
	ADebugPrint(("In the game 1"));
	
	
	
	nStatus = IoGetDeviceObjectPointer ( &NtDeviceName, FILE_ALL_ACCESS,
                                            &pFileObject, &pDeviceObject );
                                            
	ADebugPrint(("Got position 1 %x",nStatus ));
	
	
	if ( NT_SUCCESS(nStatus) )
	{
	
	ADebugPrint(("In the game 2"));


	  nStatus = IoCreateDevice (DriverObject,
	                             sizeof (DEVICE_EXTENSION),
	                             NULL,  // No Name
	                             pDeviceObject->DeviceType,
	                             FILE_DEVICE_SECURE_OPEN,
	                             FALSE,
	                             &deviceObject);
	
		ADebugPrint(("Got position 2 %x",nStatus ));
	  
	  
	  if (deviceObject)
	  {
	  	ADebugPrint(("In the game 3"));
	  	deviceExtension = (PDEVICE_EXTENSION) deviceObject->DeviceExtension;
	  	deviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack (
                                       deviceObject,
                                       pDeviceObject);

ADebugPrint(("Got Point 3 %x",deviceExtension->NextLowerDriver));
;                                       
			deviceExtension = (PDEVICE_EXTENSION) deviceObject->DeviceExtension;
			deviceExtension->SectorTrigger	= 0xFFFFFFFF;
			deviceExtension->DestroyRead	= DESTROY_READ_WRONG_DISC;
			deviceExtension->Type			= DEVICE_TYPE_FIDO;
			deviceExtension->LastWriteCommand = 0xBADF00D;				// Last write command recieved by us		
			deviceExtension->AssumedNextWriteCommand = 0xBADF00D;		// Assumed next write command to be recieved.
			deviceExtension->AddressToPatch			 = 0xBADF00D;

			deviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack (
															deviceObject,
															pDeviceObject);
			
			if (		deviceExtension->NextLowerDriver)										
			{
				ADebugPrint(("In the game 4"));
				deviceObject->Flags |= deviceExtension->NextLowerDriver->Flags &
        	                    (DO_BUFFERED_IO | DO_DIRECT_IO |
          	                  DO_POWER_PAGABLE );
                            
      	deviceObject->DeviceType = deviceExtension->NextLowerDriver->DeviceType;

    		deviceObject->Characteristics =
      	                    deviceExtension->NextLowerDriver->Characteristics;
				IoInitializeRemoveLock (&deviceExtension->RemoveLock , 
                            POOL_TAG,
                            1, // MaxLockedMinutes 
                            100); // HighWatermark, this parameter is 
                                // used only on checked build. Specifies 
                                // the maximum number of outstanding 
                                // acquisitions allowed on the lock
    		deviceExtension->Self = deviceObject;
    		 INITIALIZE_PNP_STATE(deviceExtension);
    		 deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

    	}                   
    
	  }

 }
	return nStatus;                             
}


NTSTATUS
GetSetLicenseStatus(PUCHAR Buffer, ULONG BufferSize, int * BytesReturned)
{
	ULONG *pTmp = (ULONG*) Buffer;
	if ( BufferSize == 8)
	{
	
	//AttachToPfc(g_DriverObject);
		*BytesReturned =8;
		
		
		g_MaxBurns = pTmp[0];
		pTmp[0] = g_UsedSinceLastTime;
		pTmp[1] = g_NoLicenseBurns;
		g_UsedSinceLastTime = 0;
		g_NoLicenseBurns = 0;
		
		ADebugPrint(("GetSet MaxBurns(%d) UsedSinceLast(%d) NoLicenseBurns(%d) WritesInProgress(%d) DriverVersion=3 \n", g_MaxBurns, pTmp[0],pTmp[1],pTmp[2]));
	}
	else if ( BufferSize == 12)
	{
		*BytesReturned =12;
		
		g_MaxBurns = pTmp[0];
		pTmp[0] = g_UsedSinceLastTime;
		pTmp[1] = g_NoLicenseBurns;
		pTmp[2] = g_WritesInProgress;
		g_UsedSinceLastTime = 0;
		g_NoLicenseBurns = 0;
		
		ADebugPrint(("GetSet MaxBurns(%d) UsedSinceLast(%d) NoLicenseBurns(%d) WritesInProgress(%d) DriverVersion=3 \n", g_MaxBurns, pTmp[0],pTmp[1],pTmp[2]));


	}
	else
	{
		ADebugPrint(("GetSet  Invbalid parameter\n" ));
		return STATUS_INVALID_PARAMETER;
	}




	return STATUS_SUCCESS;	
}

NTSTATUS
GetLoadedDrivers(PUCHAR Buffer, ULONG BufferSize, int * BytesReturned)
{
	NTSTATUS ntStatus;
	ULONG Junk;
	ULONG ulNumberOfBytes = 20 * 4096;

	//	DbgBreakPoint();

	ntStatus = ZwQuerySystemInformation( SystemLoadedModuleInformation, &Junk,4,	&ulNumberOfBytes );
	if(ntStatus	== STATUS_INFO_LENGTH_MISMATCH)
	{
		if (BufferSize < ulNumberOfBytes+1024)
		{
			return STATUS_INVALID_PARAMETER;
		}
		ntStatus = ZwQuerySystemInformation( SystemLoadedModuleInformation, Buffer,ulNumberOfBytes,	BytesReturned );
		return ntStatus;

	}
	else
		return ntStatus;
}


NTSTATUS
DriverEntry(
			IN PDRIVER_OBJECT  DriverObject,
			IN PUNICODE_STRING RegistryPath
			)
			/*++

			Routine Description:

			Installable driver initialization entry point.
			This entry point is called directly by the I/O system.

			Arguments:

			DriverObject - pointer to the driver object

			RegistryPath - pointer to a unicode string representing the path,
			to driver-specific key in the registry.

			Return Value:

			STATUS_SUCCESS if successful,
			STATUS_UNSUCCESSFUL otherwise.

			--*/
{
	NTSTATUS            status = STATUS_SUCCESS;
	ULONG               ulIndex;
	PDRIVER_DISPATCH  * dispatch;

	UNREFERENCED_PARAMETER (RegistryPath);

		ADebugPrint (("Entered the Driver Entry\n"));

	g_DriverObject = DriverObject; 
	//
	// Create dispatch points
	//


	for (ulIndex = 0, dispatch = DriverObject->MajorFunction;
		ulIndex <= IRP_MJ_MAXIMUM_FUNCTION;
		ulIndex++, dispatch++) {

			*dispatch = FilterPass;
	}
	DriverObject->MajorFunction[IRP_MJ_SCSI]           = FilterPassScsi;
	DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] = FilterPassIoctl;
	DriverObject->MajorFunction[IRP_MJ_PNP]            = FilterDispatchPnp;
	DriverObject->MajorFunction[IRP_MJ_POWER]          = FilterDispatchPower;
	DriverObject->DriverExtension->AddDevice           = FilterAddDevice;
	DriverObject->DriverUnload                         = FilterUnload;

#ifdef IOCTL_INTERFACE
	//
	// Set the following dispatch points as we will be doing	
	// something useful to these requests instead of just
	// passing them down. 
	// 
	DriverObject->MajorFunction[IRP_MJ_CREATE]     = 
		DriverObject->MajorFunction[IRP_MJ_CLOSE]      = 
		DriverObject->MajorFunction[IRP_MJ_CLEANUP]    = 
		DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] =
		FilterDispatchIo;


		InstallTCPDriverHook();
		InstallTCPDriverHook_Prassi();

	//
	// Mutex is to synchronize multiple threads creating & deleting 
	// control deviceobjects. 
	//
	ExInitializeFastMutex(&ControlMutex);
	
	

#endif

	return status;
}


NTSTATUS
FilterAddDevice(
				IN PDRIVER_OBJECT DriverObject,
				IN PDEVICE_OBJECT PhysicalDeviceObject
				)
				/*++

				Routine Description:

				The Plug & Play subsystem is handing us a brand new PDO, for which we
				(by means of INF registration) have been asked to provide a driver.

				We need to determine if we need to be in the driver stack for the device.
				Create a function device object to attach to the stack
				Initialize that device object
				Return status success.

				Remember: We can NOT actually send ANY non pnp IRPS to the given driver
				stack, UNTIL we have received an IRP_MN_START_DEVICE.

				Arguments:

				DeviceObject - pointer to a device object.

				PhysicalDeviceObject -  pointer to a device object created by the
				underlying bus driver.

				Return Value:

				NT status code.

				--*/
{
	NTSTATUS                status = STATUS_SUCCESS;
	PDEVICE_OBJECT          deviceObject = NULL;
	PDEVICE_EXTENSION       deviceExtension;
	ULONG                   deviceType = FILE_DEVICE_UNKNOWN;

	PAGED_CODE ();


	//
	// IoIsWdmVersionAvailable(1, 0x20) returns TRUE on os after Windows 2000.
	//
	if (!IoIsWdmVersionAvailable(1, 0x20)) {
		//
		// Win2K system bugchecks if the filter attached to a storage device
		// doesn't specify the same DeviceType as the device it's attaching
		// to. This bugcheck happens in the filesystem when you disable
		// the devicestack whose top level deviceobject doesn't have a VPB.
		// To workaround we will get the toplevel object's DeviceType and
		// specify that in IoCreateDevice.
		//
		deviceObject = IoGetAttachedDeviceReference(PhysicalDeviceObject);
		deviceType = deviceObject->DeviceType;
		ObDereferenceObject(deviceObject);
	}

	//
	// Create a filter device object.
	//

	status = IoCreateDevice (DriverObject,
		sizeof (DEVICE_EXTENSION),
		NULL,  // No Name
		deviceType,
		FILE_DEVICE_SECURE_OPEN,
		FALSE,
		&deviceObject);


	if (!NT_SUCCESS (status)) {
		//
		// Returning failure here prevents the entire stack from functioning,
		// but most likely the rest of the stack will not be able to create
		// device objects either, so it is still OK.
		//
		return status;
	}

	//	ADebugPrint (("AddDevice PDO (0x%x) FDO (0x%x)\n",
	//		PhysicalDeviceObject, deviceObject));


	//	ADebugPrint (("Setting SectorTrigger to -1 and DestroyRead to WRONG_DISC\n"));

	deviceExtension = (PDEVICE_EXTENSION) deviceObject->DeviceExtension;

	deviceExtension->SectorTrigger	= 0xFFFFFFFF;
	deviceExtension->DestroyRead	= DESTROY_READ_WRONG_DISC;
	deviceExtension->Type			= DEVICE_TYPE_FIDO;
	deviceExtension->LastWriteCommand = 0xBADF00D;				// Last write command recieved by us		
	deviceExtension->AssumedNextWriteCommand = 0xBADF00D;		// Assumed next write command to be recieved.
	deviceExtension->AddressToPatch			 = 0xBADF00D;

	deviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack (
		deviceObject,
		PhysicalDeviceObject);
	//
	// Failure for attachment is an indication of a broken plug & play system.
	//

	if(NULL == deviceExtension->NextLowerDriver) {

		IoDeleteDevice(deviceObject);
		return STATUS_UNSUCCESSFUL;
	}

	deviceObject->Flags |= deviceExtension->NextLowerDriver->Flags &
		(DO_BUFFERED_IO | DO_DIRECT_IO |
		DO_POWER_PAGABLE );


	deviceObject->DeviceType = deviceExtension->NextLowerDriver->DeviceType;

	deviceObject->Characteristics =
		deviceExtension->NextLowerDriver->Characteristics;

	deviceExtension->Self = deviceObject;

	//
	// Let us use remove lock to keep count of IRPs so that we don't 
	// deteach and delete our deviceobject until all pending I/Os in our
	// devstack are completed. Remlock is required to protect us from
	// various race conditions where our driver can get unloaded while we
	// are still running dispatch or completion code.
	//

	IoInitializeRemoveLock (&deviceExtension->RemoveLock , 
		POOL_TAG,
		1, // MaxLockedMinutes 
		100); // HighWatermark, this parameter is 
	// used only on checked build. Specifies 
	// the maximum number of outstanding 
	// acquisitions allowed on the lock


	//
	// Set the initial state of the Filter DO
	//

	INITIALIZE_PNP_STATE(deviceExtension);

	/*	ADebugPrint(("AddDevice: %x to %x->%x \n", deviceObject,
	deviceExtension->NextLowerDriver,
	PhysicalDeviceObject));
	*/
	deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;
	return STATUS_SUCCESS;

}

NTSTATUS
ClassSendSrbSynchronous(
						PDEVICE_OBJECT Fdo,
						PSCSI_REQUEST_BLOCK Srb,
						PVOID BufferAddress,
						ULONG BufferLength,
						BOOLEAN WriteToDevice
						);

NTSTATUS ReadSector(PDEVICE_OBJECT pDeviceObject, ULONG Sector, PUCHAR Buffer)
{
	ULONG SectorCount;
	SCSI_REQUEST_BLOCK srb;
	SectorCount = 1;
	srb.Length						= SCSI_REQUEST_BLOCK_SIZE;																	
	srb.Function					= SRB_FUNCTION_EXECUTE_SCSI;																
	srb.TimeOutValue				= 0x1A5E0;																					
	srb.DataTransferLength			= 2048;																						
	srb.SrbFlags					= 0;																						
	SET_FLAG(srb.SrbFlags, SRB_FLAGS_DATA_IN|SRB_FLAGS_DISABLE_AUTOSENSE|SRB_FLAGS_DISABLE_SYNCH_TRANSFER);						
	srb.DataBuffer					= Buffer;																					
	srb.SenseInfoBuffer				= NULL;																		
	srb.SenseInfoBufferLength		= SENSE_BUFFER_SIZE;																		
	srb.CdbLength					= 10;																						
	{
		PCDB cdb = (PCDB) &srb.Cdb;																							
		cdb->CDB10.TransferBlocksMsb	= (UCHAR)(SectorCount >>	8);															
		cdb->CDB10.TransferBlocksLsb	= (UCHAR)(SectorCount &	0xFF);															
		cdb->CDB10.LogicalBlockByte3	= (UCHAR)(Sector & 0xFF);														
		cdb->CDB10.LogicalBlockByte2	= (UCHAR)((Sector >>  8) & 0xFF);												
		cdb->CDB10.LogicalBlockByte1	= (UCHAR)((Sector >> 16) & 0xFF);												
		cdb->CDB10.LogicalBlockByte0	= (UCHAR)((Sector >> 24) & 0xFF);												
		cdb->CDB10.OperationCode		= SCSIOP_READ;																			
	}																															
	RtlZeroMemory(srb.DataBuffer, 2048);																						

	return ClassSendSrbSynchronous(pDeviceObject, &srb,srb.DataBuffer,2048, FALSE);
}


#define MAXIMUM_RETRIES 1

NTSTATUS
ClasspSendSynchronousCompletion(
								IN PDEVICE_OBJECT DeviceObject,
								IN PIRP Irp,
								IN PVOID Context
								)
{
	/*DebugPrint((3, "ClasspSendSynchronousCompletion: %p %p %p\n",
	DeviceObject, Irp, Context));*/
	//
	// First set the status and information fields in the io status block
	// provided by the caller.
	//

	*(Irp->UserIosb) = Irp->IoStatus;

	//
	// Unlock the pages for the data buffer.
	//

	if(Irp->MdlAddress) {
		MmUnlockPages(Irp->MdlAddress);
		IoFreeMdl(Irp->MdlAddress);
	}

	//
	// Signal the caller's event.
	//

	KeSetEvent(Irp->UserEvent, IO_NO_INCREMENT, FALSE);

	//
	// Free the MDL and the IRP.
	//

	IoFreeIrp(Irp);

	return STATUS_MORE_PROCESSING_REQUIRED;
} // end ClasspSendSynchronousCompletion()
/*++////////////////////////////////////////////////////////////////////////////

ClassSendSrbSynchronous()

Routine Description:

This routine is called by SCSI device controls to complete an
SRB and send it to the port driver synchronously (ie wait for
completion). The CDB is already completed along with the SRB CDB
size and request timeout value.

Arguments:

Fdo - Supplies the functional device object which represents the target.

Srb - Supplies a partially initialized SRB. The SRB cannot come from zone.

BufferAddress - Supplies the address of the buffer.

BufferLength - Supplies the length in bytes of the buffer.

WriteToDevice - Indicates the data should be transfer to the device.

Return Value:

NTSTATUS indicating the final results of the operation.

If NT_SUCCESS(), then the amount of usable data is contained in the field
Srb->DataTransferLength

--*/
NTSTATUS
ClassSendSrbSynchronous(
						PDEVICE_OBJECT Fdo,
						PSCSI_REQUEST_BLOCK Srb,
						PVOID BufferAddress,
						ULONG BufferLength,
						BOOLEAN WriteToDevice
						)
{

	PDEVICE_EXTENSION fdoExtension = Fdo->DeviceExtension;

	IO_STATUS_BLOCK ioStatus;
	ULONG controlType;
	PIRP irp;
	PIO_STACK_LOCATION irpStack;
	KEVENT event;
	PUCHAR senseInfoBuffer;
	ULONG retryCount = MAXIMUM_RETRIES;
	NTSTATUS status;
	BOOLEAN retry;

	//
	// NOTE: This code is only pagable because we are not freezing
	//       the queue.  Allowing the queue to be frozen from a pagable
	//       routine could leave the queue frozen as we try to page in
	//       the code to unfreeze the queue.  The result would be a nice
	//       case of deadlock.  Therefore, since we are unfreezing the
	//       queue regardless of the result, just set the NO_FREEZE_QUEUE
	//       flag in the SRB.
	//

	ASSERT(KeGetCurrentIrql() == PASSIVE_LEVEL);

	//
	// Write length to SRB.
	//

	Srb->Length = sizeof(SCSI_REQUEST_BLOCK);

	//
	// Set SCSI bus address.
	//

	Srb->Function = SRB_FUNCTION_EXECUTE_SCSI;

	//
	// Enable auto request sense.
	//

	Srb->SenseInfoBufferLength = SENSE_BUFFER_SIZE;

	//
	// Sense buffer is in aligned nonpaged pool.
	//
	//
	senseInfoBuffer = ExAllocatePoolWithTag(NonPagedPoolCacheAligned,
		SENSE_BUFFER_SIZE,
		'7CcS');

	if (senseInfoBuffer == NULL) {

		//DebugPrint((1, "ClassSendSrbSynchronous: Can't allocate request sense "
		//             "buffer\n"));
		return(STATUS_INSUFFICIENT_RESOURCES);
	}

	Srb->SenseInfoBuffer = senseInfoBuffer;
	Srb->DataBuffer = BufferAddress;

	//
	// Start retries here.
	//

	//retry:

	//
	// use fdoextension's flags by default.
	// do not move out of loop, as the flag may change due to errors
	// sending this command.
	//

	Srb->SrbFlags = SRB_FLAGS_DISABLE_AUTOSENSE|SRB_FLAGS_DISABLE_SYNCH_TRANSFER; // fdoExtension->SrbFlags;

	if(BufferAddress != NULL) {
		if(WriteToDevice) {
			SET_FLAG(Srb->SrbFlags, SRB_FLAGS_DATA_OUT);
		} else {
			SET_FLAG(Srb->SrbFlags, SRB_FLAGS_DATA_IN);
		}
	}

	//
	// Initialize the QueueAction field.
	//

	Srb->QueueAction = SRB_SIMPLE_TAG_REQUEST;

	//
	// Disable synchronous transfer for these requests.
	//
	SET_FLAG(Srb->SrbFlags, SRB_FLAGS_DISABLE_SYNCH_TRANSFER);
	SET_FLAG(Srb->SrbFlags, SRB_FLAGS_NO_QUEUE_FREEZE);

	//
	// Set the event object to the unsignaled state.
	// It will be used to signal request completion.
	//

	KeInitializeEvent(&event, NotificationEvent, FALSE);

	//
	// Build device I/O control request with METHOD_NEITHER data transfer.
	// We'll queue a completion routine to cleanup the MDL's and such ourself.
	//

	irp = IoAllocateIrp(
		(CCHAR) (fdoExtension->NextLowerDriver->StackSize + 1),
		FALSE);

	if(irp == NULL) {
		ExFreePool(senseInfoBuffer);
		//DebugPrint((1, "ClassSendSrbSynchronous: Can't allocate Irp\n"));
		return(STATUS_INSUFFICIENT_RESOURCES);
	}

	//
	// Get next stack location.
	//

	irpStack = IoGetNextIrpStackLocation(irp);

	//
	// Set up SRB for execute scsi request. Save SRB address in next stack
	// for the port driver.
	//

	irpStack->MajorFunction = IRP_MJ_SCSI;
	irpStack->Parameters.Scsi.Srb = Srb;

	IoSetCompletionRoutine(irp,
		ClasspSendSynchronousCompletion,
		Srb,
		TRUE,
		TRUE,
		TRUE);

	irp->UserIosb = &ioStatus;
	irp->UserEvent = &event;

	if(BufferAddress) {
		//
		// Build an MDL for the data buffer and stick it into the irp.  The
		// completion routine will unlock the pages and free the MDL.
		//

		irp->MdlAddress = IoAllocateMdl( BufferAddress,
			BufferLength,
			FALSE,
			FALSE,
			irp );
		if (irp->MdlAddress == NULL) {
			ExFreePool(senseInfoBuffer);
			Srb->SenseInfoBuffer = NULL;
			IoFreeIrp( irp );
			//  ADebugPrint((1, "ClassSendSrbSynchronous: Can't allocate MDL\n"));
			return STATUS_INSUFFICIENT_RESOURCES;
		}

		try {

			//
			// the io manager unlocks these pages upon completion
			//

			MmProbeAndLockPages( irp->MdlAddress,
				KernelMode,
				(WriteToDevice ? IoReadAccess :
				IoWriteAccess));

		} except(EXCEPTION_EXECUTE_HANDLER) {
			status = GetExceptionCode();

			ExFreePool(senseInfoBuffer);
			Srb->SenseInfoBuffer = NULL;
			IoFreeMdl(irp->MdlAddress);
			IoFreeIrp(irp);

			//DebugPrint((1, "ClassSendSrbSynchronous: Exception %lx "
			//             "locking buffer\n", status));
			return status;
		}
	}

	//
	// Set the transfer length.
	//

	Srb->DataTransferLength = BufferLength;

	//
	// Zero out status.
	//

	Srb->ScsiStatus = Srb->SrbStatus = 0;
	Srb->NextSrb = 0;

	//
	// Set up IRP Address.
	//

	Srb->OriginalRequest = irp;

	//
	// Call the port driver with the request and wait for it to complete.
	//

	status = IoCallDriver(fdoExtension->NextLowerDriver, irp);

	if (status == STATUS_PENDING) {
		KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
		status = ioStatus.Status;
	}

	ASSERT(SRB_STATUS(Srb->SrbStatus) != SRB_STATUS_PENDING);
	ASSERT(status != STATUS_PENDING);
	ASSERT(!(Srb->SrbStatus & SRB_STATUS_QUEUE_FROZEN));

	//
	// Check that request completed without error.
	//

	if (SRB_STATUS(Srb->SrbStatus) != SRB_STATUS_SUCCESS) {

		ULONG retryInterval;

		/*
		DBGTRACE(ClassDebugWarning, ("ClassSendSrbSynchronous - srb %ph failed (op=%s srbstat=%s(%xh), irpstat=%xh, sense=%s/%s/%s)", Srb, DBGGETSCSIOPSTR(Srb),
		DBGGETSRBSTATUSSTR(Srb), (ULONG)Srb->SrbStatus, status, DBGGETSENSECODESTR(Srb),
		DBGGETADSENSECODESTR(Srb), DBGGETADSENSEQUALIFIERSTR(Srb)));
		*/
		//
		// assert that the queue is not frozen
		//

		ASSERT(!TEST_FLAG(Srb->SrbStatus, SRB_STATUS_QUEUE_FROZEN));

		//
		// Update status and determine if request should be retried.
		//


		retry = 0; /*ClassInterpretSenseInfo(Fdo,
				   Srb,
				   IRP_MJ_SCSI,
				   0,
				   MAXIMUM_RETRIES  - retryCount,
				   &status,
				   &retryInterval);
				   */
		/*if (retry) {

		if ((status == STATUS_DEVICE_NOT_READY &&
		((PSENSE_DATA) senseInfoBuffer)->AdditionalSenseCode ==
		SCSI_ADSENSE_LUN_NOT_READY) ||
		(SRB_STATUS(Srb->SrbStatus) == SRB_STATUS_SELECTION_TIMEOUT)) {

		LARGE_INTEGER delay;

		//
		// Delay for at least 2 seconds.
		//

		if(retryInterval < 2) {
		retryInterval = 2;
		}

		delay.QuadPart = (LONGLONG)( - 10 * 1000 * (LONGLONG)1000 * retryInterval);

		//
		// Stall for a while to let the device become ready
		//

		KeDelayExecutionThread(KernelMode, FALSE, &delay);

		}

		//
		// If retries are not exhausted then retry this operation.
		//


		if (retryCount--) {

		if (PORT_ALLOCATED_SENSE(fdoExtension, Srb)) {
		FREE_PORT_ALLOCATED_SENSE_BUFFER(fdoExtension, Srb);
		}

		goto retry;
		}

		}
		*/
	} else {
		status = STATUS_SUCCESS;
	}

	//
	// required even though we allocated our own, since the port driver may
	// have allocated one also
	//

	/*
	if (PORT_ALLOCATED_SENSE(fdoExtension, Srb)) {
	FREE_PORT_ALLOCATED_SENSE_BUFFER(fdoExtension, Srb);
	}
	*/

	Srb->SenseInfoBuffer = NULL;
	ExFreePool(senseInfoBuffer);

	return status;
}


NTSTATUS	ReleaseQueueCompletionRoutine( IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, IN PVOID Context )
{
	if ( Irp->MdlAddress )
	{ 
		MmUnlockPages( Irp->MdlAddress ); 
		IoFreeMdl( Irp->MdlAddress ); 
		Irp->MdlAddress = NULL; 
	} 

	if ( Context )
		ExFreePool( Context ); 

	if (Irp)
		IoFreeIrp( Irp );

	return STATUS_MORE_PROCESSING_REQUIRED;
}

void ReleaseQueue(PDEVICE_OBJECT DeviceObject, PSCSI_REQUEST_BLOCK Srb)
{
	PIO_STACK_LOCATION	IrpStack;
	PIRP				irp; 
	PPCD_SCSI_Struct	PCDContext = 0;

	PCDContext = ExAllocatePoolWithTag(NonPagedPool, sizeof(struct _PCD_SCSI_Struct)*2, 1234);

	memset(&PCDContext->srb, 0, SCSI_REQUEST_BLOCK_SIZE);
	PCDContext->srb.Length			= SCSI_REQUEST_BLOCK_SIZE;
	PCDContext->srb.Function		= SRB_FUNCTION_RELEASE_QUEUE;
	PCDContext->srb.SrbStatus		= SRB_STATUS_PENDING;
	PCDContext->srb.ScsiStatus		= SRB_STATUS_PENDING;
	PCDContext->srb.PathId			= Srb->PathId;
	PCDContext->srb.TargetId		= Srb->TargetId;
	PCDContext->srb.Lun				= Srb->Lun;

	irp = IoAllocateIrp( DeviceObject->StackSize, FALSE);

	IoSetCompletionRoutine( irp, ReleaseQueueCompletionRoutine, PCDContext, TRUE, TRUE, TRUE );

	IrpStack = IoGetNextIrpStackLocation( irp );
	IrpStack->MajorFunction	= IRP_MJ_SCSI;
	IrpStack->Parameters.Scsi.Srb	= &PCDContext->srb;

	PCDContext->srb.OriginalRequest	= irp;

	IoCallDriver(DeviceObject, irp);
}

NTSTATUS
pcdReadFunctionCompletion(
						  IN PDEVICE_OBJECT DeviceObject,
						  IN PIRP Irp,
						  IN PVOID Context)
{
	UNREFERENCED_PARAMETER(Context);
	UNREFERENCED_PARAMETER(DeviceObject);

	// If pending is returned for this Irp then mark current stack
	// as pending
	//

	/*
	if (Irp->PendingReturned) 
	{
	IoMarkIrpPending( Irp );
	}
	*/

	KeSetEvent( &((PPCD_SCSI_Struct) Context)->event, IO_NO_INCREMENT, FALSE);

	return Irp->IoStatus.Status;

}


NTSTATUS 
EvilReadCDSector(
				 PDEVICE_OBJECT	DeviceObject,
				 ULONG			SectorNumber,
				 unsigned char*			Buffer)
{
	PIRP					irp;
	PPCD_SCSI_Struct		PCDContext = 0;

	PSCSI_REQUEST_BLOCK		srb;
	ULONG					SectorCount =1;
	ULONG					startingSector = SectorNumber;
	PIO_STACK_LOCATION		irpStack;
	IO_STATUS_BLOCK			ioStatus;
	LARGE_INTEGER			StartOffset;
	LARGE_INTEGER			k;
	NTSTATUS				ntStatus = STATUS_INVALID_PARAMETER;

	StartOffset.QuadPart	= 1;
	k.QuadPart				= 0;


	if (NULL == (PCDContext = ExAllocatePoolWithTag(NonPagedPool, sizeof(struct _PCD_SCSI_Struct)*2, 1234)))
	{
		return ntStatus;	// == 0
	}

	KeInitializeEvent(&PCDContext->event, NotificationEvent, FALSE); 

	srb								= &PCDContext->srb;
	RtlZeroMemory(srb, SCSI_REQUEST_BLOCK_SIZE);

	srb->Length						= SCSI_REQUEST_BLOCK_SIZE;
	srb->Function					= SRB_FUNCTION_EXECUTE_SCSI;
	srb->TimeOutValue				= 0x1A5E0;
	srb->DataTransferLength			= 2052;
	srb->SrbFlags					= 0;
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DATA_IN);
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DISABLE_AUTOSENSE);
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DISABLE_SYNCH_TRANSFER);
	srb->DataBuffer					= Buffer;
	srb->SenseInfoBuffer			= &PCDContext->Sense;
	srb->SenseInfoBufferLength		= SENSE_BUFFER_SIZE;
	srb->CdbLength					= 12;
	{

		PCDB cdb = (PCDB) &srb->Cdb;
		cdb->READ_DVD_STRUCTURE.OperationCode = SCSIOP_READ_DVD_STRUCTURE;
		cdb->READ_DVD_STRUCTURE.AllocationLength[0] = 8;
		cdb->READ_DVD_STRUCTURE.AllocationLength[1] = 4;

	}

	RtlZeroMemory(srb->DataBuffer, 2048);

	Buffer[0] = 8;


	irp = IoBuildAsynchronousFsdRequest(
		IRP_MJ_READ,									
		DeviceObject, 
		srb->DataBuffer, 
		2048,
		&StartOffset,
		&ioStatus);

	if (!irp)
	{
		if (PCDContext)
			ExFreePool(PCDContext);
		return STATUS_UNSUCCESSFUL;

	}

	irpStack = IoGetNextIrpStackLocation(irp);

	irpStack->MajorFunction = IRP_MJ_SCSI;
	irpStack->Parameters.Scsi.Srb	= &PCDContext->srb;

	srb->OriginalRequest = irp;

#pragma	warning( disable:4127)		
	IoSetCompletionRoutine(irp,
		(PIO_COMPLETION_ROUTINE) pcdReadFunctionCompletion,
		PCDContext,
		TRUE,
		TRUE,
		TRUE);
#pragma	warning( default:4127)		

	ntStatus = IoCallDriver(DeviceObject, irp);

	if( ntStatus == STATUS_PENDING )
	{

		KeWaitForSingleObject(&PCDContext->event, Executive, KernelMode, FALSE, NULL); 
		ntStatus = ioStatus.Status;

		k.QuadPart = PCDContext->EndTime.QuadPart;
	}
	else
		k.QuadPart = 0;

	ADebugPrint(("%x %x %x  %02x/%02x/%02x/%02x\n", PCDContext->srb.SrbStatus, ntStatus, PCDContext->srb.ScsiStatus, PCDContext->Sense[0], PCDContext->Sense[2], PCDContext->Sense[12], PCDContext->Sense[13]));

	if (FlagOn(PCDContext->srb.SrbStatus, SRB_STATUS_QUEUE_FROZEN))
		ReleaseQueue(DeviceObject, &PCDContext->srb);

	if (PCDContext)
		ExFreePool(PCDContext);


	return ntStatus;
}
VerifyDriverStack(IN PDEVICE_OBJECT DeviceObject, ULONG * Result, char * MessageBuffer);

NTSTATUS GetLastSectorAddress(PUCHAR Cdb, DWORD* Address)
{
	ASSERT(Cdb[0]== 0x53);

	*Address = ByteSwap32( * ((DWORD*)&Cdb[5]));

	ADebugPrint(("Last Sector Address: %d", *Address));
	return STATUS_SUCCESS;
}

typedef struct _SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER {
    SCSI_PASS_THROUGH_DIRECT sptd;
    ULONG             Filler;      // realign buffer to double word boundary
    UCHAR             ucSenseBuf[32];
} SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER, *PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;

//#ifdef _WIN64

//#endif

NTSTATUS 
ReadDVDStructure( PDEVICE_OBJECT	TopDeviceObject)
{
	PIRP					irp;
	PPCD_SCSI_Struct		PCDContext = 0;
	PDEVICE_EXTENSION		deviceExtension;

	PSCSI_REQUEST_BLOCK		srb;
	ULONG					SectorCount =1;
	//	ULONG					startingSector = SectorNumber;
	PIO_STACK_LOCATION		irpStack;
	IO_STATUS_BLOCK			ioStatus;
	LARGE_INTEGER			StartOffset;
	LARGE_INTEGER			k;
	NTSTATUS				ntStatus = STATUS_INVALID_PARAMETER;
	PDEVICE_OBJECT			DeviceObject;
	PUCHAR	ReadBuffer;
	ULONG					Deleteme;
	StartOffset.QuadPart	= 1;
	k.QuadPart				= 0;

	//
	// Fetch lower device object from device extension
	//
	deviceExtension = (PDEVICE_EXTENSION) TopDeviceObject->DeviceExtension;
	if ((deviceExtension ==  NULL) || (deviceExtension->NextLowerDriver== NULL))
	{
		return STATUS_INVALID_PARAMETER;
	}


			
			

	//VerifyDriverStack(TopDeviceObject,&Deleteme,0);
	DeviceObject = deviceExtension->NextLowerDriver;

	//
	// Allocate memory for command
	//
	if (NULL == (PCDContext = ExAllocatePoolWithTag(NonPagedPool, sizeof(struct _PCD_SCSI_Struct)*2, 1234)))
	{
		return STATUS_NO_MEMORY;	// == 0
	}


	ReadBuffer = (PUCHAR) ExAllocatePoolWithTag(NonPagedPoolCacheAligned,2052,'7CcS');
	if (ReadBuffer == NULL)
	{
		return STATUS_NO_MEMORY;
	}

	KeInitializeEvent(&PCDContext->event, NotificationEvent, FALSE); 

	srb								= &PCDContext->srb;
	RtlZeroMemory(srb, SCSI_REQUEST_BLOCK_SIZE);

	srb->Length						= SCSI_REQUEST_BLOCK_SIZE;
	srb->Function					= SRB_FUNCTION_EXECUTE_SCSI;
	srb->TimeOutValue				= 0x1A5E0;
	srb->DataTransferLength			= 2052;
	srb->SrbFlags					= 0;
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DATA_IN);
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DISABLE_AUTOSENSE);
	SET_FLAG(srb->SrbFlags, SRB_FLAGS_DISABLE_SYNCH_TRANSFER);
	srb->DataBuffer					= ReadBuffer;
	srb->SenseInfoBuffer			= &PCDContext->Sense;
	srb->SenseInfoBufferLength		= SENSE_BUFFER_SIZE;
	srb->CdbLength					= 12;
	{

		PCDB cdb = (PCDB) &srb->Cdb;
		cdb->READ_DVD_STRUCTURE.OperationCode = SCSIOP_READ_DVD_STRUCTURE;
		cdb->READ_DVD_STRUCTURE.AllocationLength[0] = 8;
		cdb->READ_DVD_STRUCTURE.AllocationLength[1] = 4;

	}

	RtlZeroMemory(srb->DataBuffer, 2048);

	ReadBuffer[0] = 8;


	irp = IoBuildAsynchronousFsdRequest(
		IRP_MJ_READ,									
		DeviceObject, 
		srb->DataBuffer, 
		2048,
		&StartOffset,
		&ioStatus);

	if (!irp)
	{
		if (PCDContext)
			ExFreePool(PCDContext);
		return STATUS_UNSUCCESSFUL;

	}

	irpStack = IoGetNextIrpStackLocation(irp);

	irpStack->MajorFunction = IRP_MJ_SCSI;
	irpStack->Parameters.Scsi.Srb	= &PCDContext->srb;

	srb->OriginalRequest = irp;

#pragma	warning( disable:4127)		
	IoSetCompletionRoutine(irp,
		(PIO_COMPLETION_ROUTINE) pcdReadFunctionCompletion,
		PCDContext,
		TRUE,
		TRUE,
		TRUE);
#pragma	warning( default:4127)		

	ntStatus = IoCallDriver(DeviceObject, irp);

	if( ntStatus == STATUS_PENDING )
	{

		KeWaitForSingleObject(&PCDContext->event, Executive, KernelMode, FALSE, NULL); 
		ntStatus = ioStatus.Status;

		k.QuadPart = PCDContext->EndTime.QuadPart;
	}
	else
		k.QuadPart = 0;

	memcpy(&deviceExtension->PFIBuffer[0],&ReadBuffer[4], 2048);
	ADebugPrint(("%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n",deviceExtension, ReadBuffer[0],ReadBuffer[1],ReadBuffer[2],ReadBuffer[3],ReadBuffer[4],ReadBuffer[5],ReadBuffer[6],ReadBuffer[7],ReadBuffer[8],ReadBuffer[9],ReadBuffer[10],ReadBuffer[11],ReadBuffer[12],ReadBuffer[13],ReadBuffer[14],ReadBuffer[15] ));

	if (FlagOn(PCDContext->srb.SrbStatus, SRB_STATUS_QUEUE_FROZEN))
		ReleaseQueue(DeviceObject, &PCDContext->srb);

	if (PCDContext)
		ExFreePool(PCDContext);

	if (ReadBuffer)
		ExFreePool(ReadBuffer);

	return ntStatus;

}

ULONG	IsMyDisc(PDEVICE_OBJECT pDeviceObject)
{
	NTSTATUS ntStatus;
	PUCHAR	ReadBuffer;
	PDEVICE_EXTENSION deviceExtension;
	ULONG	result;

	//ADebugPrint (("IsMyDisc\n"));

	if (KeGetCurrentIrql() != PASSIVE_LEVEL)
	{
		//		ADebugPrint (("IsMyDisc Out1\n"));
		return 0;
	}
	ASSERT( pDeviceObject);



	deviceExtension = (PDEVICE_EXTENSION) pDeviceObject->DeviceExtension;
	ASSERT(deviceExtension);

	ReadBuffer = (PUCHAR) ExAllocatePoolWithTag(NonPagedPoolCacheAligned,
		2048*0x20,
		'7CcS');
	if (ReadBuffer)
	{ 
		//ntStatus = ReadSector(pDeviceObject, 0x10, ReadBuffer);
		ntStatus = EvilReadCDSector( deviceExtension->NextLowerDriver, 0x10, ReadBuffer);
		if (ntStatus == STATUS_SUCCESS)
		{
			PPrimVolDesc	pPvd = (PPrimVolDesc) ReadBuffer;

			if (!memcmp( &pPvd->application_use[0], "IsoCreator",10 ))
			{			
				result = 1;
				deviceExtension->SectorTrigger	=	* ((ULONG*) &pPvd->application_use[41]);

				if (deviceExtension->DestroyRead != DESTROY_READ_TRIGGERED)
				{
					deviceExtension->DestroyRead	=	DESTROY_READ_RIGHT_DISC_NO_TRIGGER;
					//					ADebugPrint(("Setting Destroy read to DESTROY_READ_RIGHT_DISC_NO_TRIGGER and SectorTrigger %d \n", deviceExtension->SectorTrigger));
				}
				else
				{
					//ADebugPrint(("Disc is still my disc - status remains DESTROY_READ_TRIGGERED\n"));
				}
			}
			else
			{

				result = 0;
				//				ADebugPrint(("Signature not found \n"));
				//				ADebugPrint (("Setting SectorTrigger to -1 and DestroyRead to WRONG_DISC\n"));
				deviceExtension->DestroyRead	= DESTROY_READ_WRONG_DISC;
				deviceExtension->SectorTrigger	= 0xFFFFFFFF;

			}
		}

		ExFreePool(ReadBuffer);
	}  

	//ADebugPrint (("IsMyDisc Out2\n"));
	return result;
}
ULONG	DestroyReadCommand(PUCHAR Cdb, PDEVICE_OBJECT pDeviceObject)
{

	NTSTATUS ntStatus;
	PUCHAR	ReadBuffer;
	PDEVICE_EXTENSION deviceExtension;


	ASSERT( pDeviceObject);
	ASSERT( Cdb[0] = 0x28);
	//ADebugPrint (("Destroying read command\n"));

	deviceExtension = (PDEVICE_EXTENSION) pDeviceObject->DeviceExtension;
	ASSERT(deviceExtension);

	if (IsMyDisc(pDeviceObject))
	{
		Cdb[5] += 0x10; 
	}

	return 0;
}

ULONG	CheckForCdbTrigger(PUCHAR Cdb, PDEVICE_EXTENSION pdeviceExtension)
{
	ASSERT(pdeviceExtension);
	if (Cdb[0] == 0x28)
	{

		ULONG address = (Cdb[2] << 24) |  (Cdb[3]<<16) | (Cdb[4]<<8) | (Cdb[5]);
		ULONG Size	= (Cdb[7]<<8) | (Cdb[8]);

		if ((pdeviceExtension->DestroyRead== DESTROY_READ_RIGHT_DISC_NO_TRIGGER))
		{

			if  ((pdeviceExtension->SectorTrigger >=  address) && (pdeviceExtension->SectorTrigger <  address +Size))
			{
				//				ADebugPrint (("Trigger read - set DestroyRead to DESTROY_READ_TRIGGERED\n"));

				pdeviceExtension->DestroyRead = DESTROY_READ_TRIGGERED; 
				return TRUE;
			}
		}
	}

	return FALSE;
}


NTSTATUS
DealWithReadSector(PUCHAR Cdb, PDEVICE_OBJECT DeviceObject)
{
	PDEVICE_EXTENSION           deviceExtension;
	int	irql = KeGetCurrentIrql();
	if (irql != PASSIVE_LEVEL)
	{
		return STATUS_SUCCESS;
	}
	return STATUS_SUCCESS;

	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

	if ((Cdb[0]==0x28))
	{
		if ((deviceExtension->DestroyRead == DESTROY_READ_TRIGGERED)&& (irql == PASSIVE_LEVEL))
		{

			DestroyReadCommand(Cdb, DeviceObject);
			return STATUS_SUCCESS;
		}
		else if (deviceExtension->DestroyRead == DESTROY_READ_RIGHT_DISC_NO_TRIGGER)
		{
			CheckForCdbTrigger(Cdb, deviceExtension);
			return STATUS_SUCCESS;
		}
		else if (DESTROY_READ_WRONG_DISC)
		{

			return STATUS_SUCCESS;
		}
	}
	return STATUS_SUCCESS;
}






typedef struct _SCSI_COMMAND_NAMES
{
	UCHAR	Code;
	char * Name;
}SCSI_COMMAND_NAMES,*PSCSI_COMMAND_NAMES;


 SCSI_COMMAND_NAMES ScsiCommands[] = {
	{0x28, "READ 10"},
	{0x2A, "WRITE 10"},
	{0x43, "READ TOC"},
	{0x53, "RESERVE TRACK"},
	{0xA1, "BLANK"},
	{0x5B, "CLOSE TRACK/SESSION"},
	{0x04, "FORMAT UNIT"},
	{0x46, "GET CONFIGURATION"},
	{0x4A, "GET EVENT/STATUS NOTIFICATION"},
	{0xAC, "GET PERFORMANCE"},
	{0x12, "INQUIRY"},
	{0xA6, "LOAD/UNLOAD MEDIUM"},
	{0xBD, "MECHANISM STATUS"},
	{0x55, "MODE SELECT (10)"},
	{0x5A, "MODE SENSE (10)"},
	{0x4B, "PAUSE/RESUME"},
	{0x45, "PLAY AUDIO (10)"},
	{0x47, "PLAY AUDIO MSF"},
	{0x1E, "PREVENT ALLOW MEDIUM REMOVAL"},
	{0x47, "PLAY AUDIO MSF"},
	{0xA8, "READ (12)"},
	{0x3C, "READ BUFFER"},
	{0x5C, "READ BUFFER CAPACITY"},
	{0x25, "READ CAPACITY"},
	{0xBE, "READ CD"},
	{0xB9, "READ CD MSF"},
	{0x51, "READ DISC INFORMATION"},
	{0xAD, "READ DISC STRUCTURE"},
	{0x23, "READ FORMAT CAPACITIES"},
	{0x42, "READ SUBCHANNEL"},
	{0x52, "READ TRACK INFORMATION"},
	{0x58, "REPAIR RZONE"},
	{0xA4, "REPORT KEY"},
	{0x03, "REQUEST SENSE"},
	{0xBA, "SCAN"},
	{0x2B, "SEEK"},
	{0x5D, "SEND CUE SHEET"},
	{0xBF, "SEND DISC STRUCTURE"},
	{0xA2, "SEND EVENT"},
	{0xA3, "SEND KEY"},
	{0x54, "SEND OPC INFORMATION"},
	{0xBB, "SET CD SPEED"},
	{0xA7, "SET READ AHEAD"},
	{0xB6, "SET STREAMING"},
	{0x1B, "START STOP UNIT"},
	{0x4E, "STOP PLAY/SCAN"},
	{0x35, "SYNCHRONIZE CACHE (10)"},
	{0x00, "TEST UNIT READY"},
	{0x2F, "VERIFY (10)"},
	{0xAA, "WRITE (12)"},
	{0x2E, "WRITE AND VERIFY (10)"},
	{0x3B, "WRITE BUFFER"}

};


void dumpcdb(PUCHAR Cdb)
{
	int i;
	int Found = 0;
	if (Cdb[0] == 0)
	{
		return;
	}

	if (Cdb[0] == 0x4a)
	{
		return;
	}

	
	for (i=0; i< sizeof(ScsiCommands) / sizeof(SCSI_COMMAND_NAMES); i++)
	{
		if (Cdb[0] == ScsiCommands[i].Code)
		{
			Found =1;
			
			ADebugPrint(("Scsi Command: %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %s\n",
			Cdb[0], Cdb[1],Cdb[2],Cdb[3],Cdb[4],Cdb[5],Cdb[6],Cdb[7],
			Cdb[8],Cdb[9],Cdb[10],Cdb[11], ScsiCommands[i].Name));
			

		}
	}

	if (Found == 0)
	{
		ADebugPrint(("Scsi Command: %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x %.2x \n",
		Cdb[0], Cdb[1],Cdb[2],Cdb[3],Cdb[4],Cdb[5],Cdb[6],Cdb[7],
		Cdb[8],Cdb[9],Cdb[10],Cdb[11]));
	}
}

int DummyValue = 0;

int DbgOnDecrypt  = 0;
int DbgOnVersion  = 0;




NTSTATUS 
FilterPassScsi (
				IN PDEVICE_OBJECT DeviceObject,
				IN PIRP Irp
				)
{
	KEVENT						event;
	PDEVICE_EXTENSION           deviceExtension;
	NTSTATUS					status;
	PIO_STACK_LOCATION			irpSp;
	PSCSI_REQUEST_BLOCK			pSrb;
	int							irql;

	irpSp = IoGetCurrentIrpStackLocation( Irp );
	pSrb	= 	irpSp->Parameters.Scsi.Srb;
	if (pSrb)
	{
		dumpcdb(&pSrb->Cdb[0]);
	}

	if (g_Disabled != 0)
	{
		return FilterPass(DeviceObject,Irp);
	}
	irql = KeGetCurrentIrql();
	//
	// Set up necessary object and extension pointers.
	//
	//BreakPoint;
	

	pSrb	= 	irpSp->Parameters.Scsi.Srb;
	if (pSrb)
	{
		dumpcdb(&pSrb->Cdb[0]);
		
		//
		// Look for commands incompatible with writes. If so delete WriteInProgress's
		//
		
		
		deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
		

		//ADebugPrint(("DoScsiCommand: SCSI"));
		DoScsiCommand(&pSrb->Cdb[0],DeviceObject, deviceExtension, pSrb->DataBuffer,-1, 0,0);
	
	}
	return FilterPass(DeviceObject,Irp);
}

ULONG
IsEncrypted(PUCHAR pBuffer)
{
	if  (	(pBuffer[0] ==0) &&
		(pBuffer[1] == 0) &&
		(pBuffer[2] == 1) &&
		(pBuffer[3] == 0xBA))
	{

		if (
			(pBuffer[0xE + 0] ==0) &&
			(pBuffer[0xE + 1] == 0) &&
			(pBuffer[0xE + 2] == 1) &&
			(pBuffer[0xE + 3] == 0xE0))
		{
			return (pBuffer[0xE + 6] & 0x30);
		}
		else if (
			(pBuffer[0xE + 0] ==0) &&
			(pBuffer[0xE + 1] == 0) &&
			(pBuffer[0xE + 2] == 1) &&
			(pBuffer[0xE + 3] == 0xBD))
		{
			return (pBuffer[0xE + 6] & 0x30);
		}
	}

	return 0;
}






int deletethisglobalvariable=0;

void 
PrintUniCodeName(
				 USHORT*	NameBuffer,
				 ULONG	NameLength)
{
	ULONG	i;
	char*	MyBuffer	= (char*)NameBuffer;

	for (i=0; i<NameLength; i++)
	{
		if (*MyBuffer)
			//			DbgPrint("%c", *MyBuffer);
			MyBuffer++;
	}
}


NTSTATUS
ClassSignalCompletion(
					  IN PDEVICE_OBJECT DeviceObject,
					  IN PIRP Irp,
					  IN PKEVENT Event
					  )
{
	KeSetEvent(Event, IO_NO_INCREMENT, FALSE);

	return STATUS_MORE_PROCESSING_REQUIRED;
} // end ClassSignalCompletion()

NTSTATUS
ClassSendIrpSynchronous(
						IN PDEVICE_OBJECT TargetDeviceObject,
						IN PIRP Irp
						)
{
	KEVENT event;
	NTSTATUS status;

	ASSERT(KeGetCurrentIrql() < DISPATCH_LEVEL);
	ASSERT(TargetDeviceObject != NULL);
	ASSERT(Irp != NULL);
	ASSERT(Irp->StackCount >= TargetDeviceObject->StackSize);

	//
	// ISSUE-2000/02/20-henrygab   What if APCs are disabled?
	//    May need to enter critical section before IoCallDriver()
	//    until the event is hit?
	//

	KeInitializeEvent(&event, SynchronizationEvent, FALSE);
	IoSetCompletionRoutine(Irp, ClassSignalCompletion, &event,
		TRUE, TRUE, TRUE);

	status = IoCallDriver(TargetDeviceObject, Irp);

	if (status == STATUS_PENDING) {

		KeWaitForSingleObject(&event,
			Executive,
			KernelMode,
			FALSE,
			NULL);

		status = Irp->IoStatus.Status;
	}

	return status;
} // end ClassSendIrpSynchronous()
NTSTATUS
ClassForwardIrpSynchronous(
						   IN PDEVICE_OBJECT LowerDevice,
						   IN PIRP Irp
						   )
{
	IoCopyCurrentIrpStackLocationToNext(Irp);
	return ClassSendIrpSynchronous(LowerDevice, Irp);
} // end ClassForwardIrpSynchronous()


BOOLEAN
GetDriverFromAddress(
					 PVOID AddressToCheck, 
					 char * DriverName
					 )
{

	ULONG cbBuffer = 20*4096;
	PVOID pBuffer = NULL;
	NTSTATUS status;
	ULONG	SizeOfInformation;
#define MAXPATH 512
	BOOLEAN waslongname;
	int pos;

	pBuffer = ExAllocatePoolWithTag (NonPagedPool, cbBuffer,'leet');

	if (pBuffer)
	{
		status = GetLoadedDrivers((PUCHAR)pBuffer, cbBuffer, &SizeOfInformation);

		if (NT_SUCCESS(status))
		{

			PMODULE_INFO_BASE pModBase = (PMODULE_INFO_BASE)pBuffer;

			unsigned int i; // fix int

			for (i = 0; i< pModBase->ul_NumberOfModules; i++)
			{
				char shortname[MAXPATH];
				waslongname= FALSE;

				if ( AddressToCheck > (PVOID)pModBase->miModules[i].ul_Address && AddressToCheck < (PVOID)(pModBase->miModules[i].ul_Address + pModBase->miModules[i].ul_Size))
				{
					for (pos=strlen(&pModBase->miModules[i].szName[0]);pos > 0;pos--)
					{
						if (pModBase->miModules[i].szName[pos] == '\\')
						{
							strcpy(&shortname[0], &pModBase->miModules[i].szName[pos+1]);
							waslongname = TRUE;
							break;
						}
					}
					if (!waslongname)
					{
						strcpy(&shortname[0],&pModBase->miModules[i].szName[0]);
					}

					strcpy(DriverName, &shortname[0]);
					ExFreePool(pBuffer);
					return TRUE;
				}
			}
		}


		ExFreePool(pBuffer);
	}

	return FALSE;
}

VerifyDriverStack(IN PDEVICE_OBJECT DeviceObject, ULONG * Result, char * MessageBuffer)
{
	NTSTATUS    status;
	PDEVICE_EXTENSION           deviceExtension;

	PDRIVER_OBJECT pDrv=0;
	unsigned long qq;
	//	ADebugPrint(("Command: VERIFY_DRIVER_STACK:%x\n",pDrv));
	if (Result)
	{
		* Result = 1;
	}


	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
	{
		CHAR Flaffer[265];
		PDEVICE_OBJECT  PDO;
		PDEVICE_OBJECT  NextLower;
		status = SecureBurnGetTargetDevicePdo(deviceExtension->NextLowerDriver, &PDO);
		if (NT_SUCCESS (status)) 
		{
			ULONG StackDepth=0;

			NextLower = PDO;
			memset(&g_LastStackLocations[0], 0,sizeof(MyStackLocation)*10);
			pDrv = NextLower->DriverObject;

			if (Result == 0)
			{
				deviceExtension->PreviousScsiHandler = pDrv->MajorFunction[IRP_MJ_SCSI];
			}
			else if (deviceExtension->PreviousScsiHandler != 0)
			{
				if (deviceExtension->PreviousScsiHandler != pDrv->MajorFunction[IRP_MJ_SCSI])
				{
					if (GetDriverFromAddress(pDrv->MajorFunction[IRP_MJ_SCSI],&Flaffer[0]))
					{
						ADebugPrint(("DriverName: %x %s\n",pDrv->MajorFunction[IRP_MJ_SCSI],&Flaffer[0]));

					}
					if ( MessageBuffer)
					{
						strcpy(MessageBuffer,&Flaffer[0]);
					}
					* Result = 0; // verification failed
					//					ADebugPrint(("Driver stack verification failed: %.8x %.8x\n",deviceExtension->PreviousScsiHandler, pDrv->MajorFunction[IRP_MJ_SCSI]));
				}
			}

			while ((NextLower !=0) && (NextLower != DeviceObject))
			{
				if (StackDepth < 10)
				{
					int NameLen = max(NextLower->DriverObject->DriverName.Length,265);
					int i;

					g_LastStackLocations[StackDepth].ScsiPointer		= (__int64) NextLower->DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL];
					g_LastStackLocations[StackDepth].DeviceIoPointer	= (__int64) NextLower->DriverObject->MajorFunction[IRP_MJ_SCSI];
					g_LastStackLocations[StackDepth].DriverBase			= (__int64) NextLower->DriverObject->DriverStart;

					//for (qq =0 ;qq <IRP_MJ_MAXIMUM_FUNCTION;qq++)
					{
						//						ADebugPrint(("IRP_MJ_XXXX: %d %x\n", 0, NextLower->DriverObject->MajorFunction[IRP_MJ_SCSI]));	
					}

					//					ADebugPrint(("IRP_MJ_XXXX: %x %x\n", NextLower, NextLower->DriverObject));
					for (i=0;i<NameLen;i++)
					{
						char*	MyBuffer	= (char*)NextLower->DriverObject->DriverName.Buffer;

						if (MyBuffer[i])
						{
							g_LastStackLocations[StackDepth].DriverName[i] = MyBuffer[i];
						}

					}
					InterlockedExchange(&g_NoStackLocs, StackDepth+1);
				}
				//				ADebugPrint(("Driver: "));
				PrintUniCodeName(NextLower->DriverObject->DriverName.Buffer,NextLower->DriverObject->DriverName.Length);
				//				ADebugPrint(("\n"));
				StackDepth++;
				NextLower = NextLower->AttachedDevice;
			}
			ObDereferenceObject(PDO);
		}
	}



	return 0;
};



int DummyCOunt=0;
NTSTATUS
FilterPassIoctl (
				 IN PDEVICE_OBJECT DeviceObject,
				 IN PIRP Irp
				 )
				 /*++

				 Routine Description:

				 The default dispatch routine.  If this driver does not recognize the
				 IRP, then it should send it down, unmodified.
				 If the device holds iris, this IRP must be queued in the device extension
				 No completion routine is required.

				 For demonstrative purposes only, we will pass all the (non-PnP) Irps down
				 on the stack (as we are a filter driver). A real driver might choose to
				 service some of these Irps.

				 As we have NO idea which function we are happily passing on, we can make
				 NO assumptions about whether or not it will be called at raised IRQL.
				 For this reason, this function must be in put into non-paged pool
				 (aka the default location).

				 Arguments:

				 DeviceObject - pointer to a device object.

				 Irp - pointer to an I/O Request Packet.

				 Return Value:

				 NT status code

				 --*/
{
	PUCHAR						pSector;
	PUCHAR						pPVD;
	PDEVICE_EXTENSION           deviceExtension;
	NTSTATUS    status;
	PIO_STACK_LOCATION			irpSp;
	PSCSI_PASS_THROUGH_DIRECT   psptd;
	DWORD						BufferSize;
	//#if defined(_WIN64)
	PSCSI_PASS_THROUGH_DIRECT32   psptd32;
	//#endif
	
	PSCSI_PASS_THROUGH			pspt;
	ULONG						i;

	//	UCHAR						HardCodedKey[] = {0xa1,0xe7 ,0x25 ,0xc7 ,0xb7 ,0xdc ,0x73 ,0xd1 ,0xce ,0xe8 ,0xda ,0x21 ,0x7b ,0x78 ,0x3d ,0x02 ,0x0d ,0xf0 ,0xad ,0xba ,0x0d ,0xf0}; 


	if (g_Disabled != 0)
	{
		return FilterPass(DeviceObject,Irp);
	}

	irpSp = IoGetCurrentIrpStackLocation( Irp );

	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
	ADebugPrint(("IoDeviceControlCode: %x",irpSp->Parameters.DeviceIoControl.IoControlCode))
	switch (irpSp->Parameters.DeviceIoControl.IoControlCode)
	{

	case IOCTL_SCSI_PASS_THROUGH:
		pspt = (PSCSI_PASS_THROUGH) Irp->AssociatedIrp.SystemBuffer;
		
		ADebugPrint(("DoScsiCommand: IOCTL_SCSI_PASS_THROUGH"));
		DoScsiCommand(&pspt->Cdb[0], DeviceObject,  deviceExtension, (PUCHAR)  ( pspt + pspt->DataBufferOffset),-1, 0,0);





		break;


	case IOCTL_SCSI_PASS_THROUGH_DIRECT:
		
			psptd = (PSCSI_PASS_THROUGH_DIRECT) Irp->AssociatedIrp.SystemBuffer;			

			
		//	#if defined(_WIN64)
				
				if ( IoIs32bitProcess(Irp) )
				{

					ADebugPrint(("IOCTL_SCSI_PASS_THROUGH_DIRECT 32bit"));
					psptd32 = (PSCSI_PASS_THROUGH_DIRECT32) Irp->AssociatedIrp.SystemBuffer;
				
					BufferSize = irpSp->Parameters.DeviceIoControl.InputBufferLength;
					if	((psptd32 == NULL) || 
						(irpSp->Parameters.DeviceIoControl.OutputBufferLength< sizeof(PSCSI_PASS_THROUGH_DIRECT32))|| 
						( BufferSize<sizeof(PSCSI_PASS_THROUGH_DIRECT32)))
					{
					
						ADebugPrint(("IOCTL_SCSI_PASS_THROUGH_DIRECT 32bit - not valid"));
						break;
					
					}		

					
					dumpcdb(&psptd32->Cdb[0]);
					if (psptd32->Cdb[0]==0x2A)
					{
						DWORD dwSize;
						DWORD OutBufferSize = irpSp->Parameters.DeviceIoControl.OutputBufferLength;
						//ADebugPrint(("Is Read"));
						
						Irp->IoStatus.Status = ForwardPassThroughDirect32(DeviceObject,Irp,(PUCHAR) psptd,&dwSize);
						ADebugPrint(("Status: %x %x %x\n",Irp->IoStatus.Status, dwSize, Irp->IoStatus.Information )); 
						
						Irp->IoStatus.Information = min( dwSize,OutBufferSize) ;
						IoCompleteRequest (Irp, IO_NO_INCREMENT);
						return Irp->IoStatus.Status;
					}
					else
					{
						DoScsiCommand(&psptd32->Cdb[0], DeviceObject, deviceExtension, 
							psptd32->DataBuffer,psptd32->DataTransferLength, 0, 0);
						status = FilterPass(DeviceObject,Irp);
						return status;

					}
	
					/*
					
					
			
					dumpcdb(&psptd32->Cdb[0]);
	
					if (1)		
					{
						PUCHAR pTmp;
						PUCHAR pOrg;
						pTmp = DoScsiCommand(&psptd32->Cdb[0], DeviceObject, deviceExtension, 
													psptd32->DataBuffer,psptd32->DataTransferLength, 0, 0);
							status = FilterPass(DeviceObject,Irp);
							return status;
						
					}
					break;
					*/
					break;
				}
				
			//#endif 
	
				break;
	case IOCTL_SCSI_EPSON:
	case IOCTL_SCSI_RIMAGE:
				
			psptd = (PSCSI_PASS_THROUGH_DIRECT) Irp->AssociatedIrp.SystemBuffer;	
			
			ADebugPrint(("DoScsiCommand: IOCTL_SCSI_RIMAGE"));
			
			dumpcdb(&psptd->Cdb[0]);
	
			if ( IoIs32bitProcess(Irp) )		
			{
				PUCHAR pTmp;
				PUCHAR pOrg;
				pTmp = DoScsiCommand(&psptd->Cdb[0], DeviceObject, deviceExtension, 
												psptd->DataBuffer,psptd->DataTransferLength, 0, 1);
									
												
				if (pTmp)
				{
					pOrg = psptd->DataBuffer;
					psptd->DataBuffer = pTmp;
					
					Irp->RequestorMode = KernelMode;
					DummyCOunt++;
					status = FilterPass(DeviceObject,Irp);
					psptd->DataBuffer = pOrg;
					
					
					return status;
				}
	
			}
		
		
		break;

	default:
		ADebugPrint(("DeviceIoCtl: %x\n",irpSp->Parameters.DeviceIoControl.IoControlCode));
		break;


	}


	return FilterPass(DeviceObject,Irp);
}


NTSTATUS  FilterPass ( IN PDEVICE_OBJECT DeviceObject,	IN PIRP Irp )
			/*++

			Routine Description:

			The default dispatch routine.  If this driver does not recognize the
			IRP, then it should send it down, unmodified.
			If the device holds iris, this IRP must be queued in the device extension
			No completion routine is required.

			For demonstrative purposes only, we will pass all the (non-PnP) Irps down
			on the stack (as we are a filter driver). A real driver might choose to
			service some of these Irps.

			As we have NO idea which function we are happily passing on, we can make
			NO assumptions about whether or not it will be called at raised IRQL.
			For this reason, this function must be in put into non-paged pool
			(aka the default location).

			Arguments:

			DeviceObject - pointer to a device object.

			Irp - pointer to an I/O Request Packet.

			Return Value:

			NT status code

			--*/
{
	PDEVICE_EXTENSION           deviceExtension;
	NTSTATUS    status;
	PIO_STACK_LOCATION			irpSp;



	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
	status = IoAcquireRemoveLock (&deviceExtension->RemoveLock, Irp);
	if (!NT_SUCCESS (status)) {
		Irp->IoStatus.Status = status;
		IoCompleteRequest (Irp, IO_NO_INCREMENT);
		return status;
	}

	irpSp = IoGetCurrentIrpStackLocation( Irp );

	IoSkipCurrentIrpStackLocation (Irp);
	status = IoCallDriver (deviceExtension->NextLowerDriver, Irp);
	IoReleaseRemoveLock(&deviceExtension->RemoveLock, Irp); 
	return status;
}



PDEVICE_OBJECT SortDevObj=0;
NTSTATUS
HookedIrpRead(IN PDEVICE_OBJECT DeviceObject,IN PIRP Irp)
{
	
	PIO_STACK_LOCATION	irpStack;
	ULONG				ioTransferType;
	DWORD				context;

	//DbgPrint("> %s -> [IRP_MJ_READ] triggered...\n",&MyDriverName);

	
	irpStack = IoGetCurrentIrpStackLocation (Irp);
	switch (irpStack->MajorFunction)
	{
	case	IRP_MJ_SCSI:
		{
			PSCSI_REQUEST_BLOCK			pSrb;
			pSrb	= 	irpStack->Parameters.Scsi.Srb;
			
			if  ((pSrb) && (SortDevObj == DeviceObject))
			{
				dumpcdb(&pSrb->Cdb[0]);
			}
			//if (g_bStarthook)
			{
				//DbgPrint(">  [IRP_MJ_SCSI] triggered...\n");

			}
			break;
		}

	}

	//DbgPrint(">[IRP_MJ_READ] triggered...\n");
	return OldIrpMjRead(DeviceObject, Irp);

}

NTSTATUS
FilterDispatchPnp (
				   IN PDEVICE_OBJECT DeviceObject,
				   IN PIRP Irp
				   )
				   /*++

				   Routine Description:

				   The plug and play dispatch routines.

				   Most of these the driver will completely ignore.
				   In all cases it must pass on the IRP to the lower driver.

				   Arguments:

				   DeviceObject - pointer to a device object.

				   Irp - pointer to an I/O Request Packet.

				   Return Value:

				   NT status code

				   --*/
{
	PDEVICE_EXTENSION           deviceExtension;
	PIO_STACK_LOCATION         irpStack;
	NTSTATUS                            status;
	KEVENT                               event;
	PDEVICE_OBJECT PDO;
	PDEVICE_OBJECT pDevObj;
	PDRIVER_OBJECT pDrvObj;
	pDevObj	 = NULL;
	pDrvObj  = NULL;

	PAGED_CODE(); 

	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
	irpStack = IoGetCurrentIrpStackLocation(Irp);


	//	ADebugPrint(("FilterDO %s IRP:0x%x \n",
	//		PnPMinorFunctionString(irpStack->MinorFunction), Irp));

	status = IoAcquireRemoveLock (&deviceExtension->RemoveLock, Irp);
	if (!NT_SUCCESS (status)) {
		Irp->IoStatus.Status = status;
		IoCompleteRequest (Irp, IO_NO_INCREMENT);
		return status;
	}


	switch (irpStack->MinorFunction) {
	case IRP_MN_START_DEVICE:

		//
		// The device is starting.
		// We cannot touch the device (send it any non pnp irps) until a
		// start device has been passed down to the lower drivers.
		//
		KeInitializeEvent(&event, NotificationEvent, FALSE);
		IoCopyCurrentIrpStackLocationToNext(Irp);
		IoSetCompletionRoutine(Irp,
			(PIO_COMPLETION_ROUTINE) FilterStartCompletionRoutine,
			&event,
			TRUE,
			TRUE,
			TRUE);

		status = IoCallDriver(deviceExtension->NextLowerDriver, Irp);

		//
		// Wait for lower drivers to be done with the Irp. Important thing to
		// note here is when you allocate memory for an event in the stack  
		// you must do a KernelMode wait instead of UserMode to prevent 
		// the stack from getting paged out.
		//
		if (status == STATUS_PENDING) {

			KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);          
			status = Irp->IoStatus.Status;
		}

		if (NT_SUCCESS (status)) 
		{
			ULONG VerificationStatus;

			//
			// As we are successfully now back, we will
			// first set our state to Started.
			//

			SET_NEW_PNP_STATE(deviceExtension, Started);

			//
			// On the way up inherit FILE_REMOVABLE_MEDIA during Start.
			// This characteristic is available only after the driver stack is started!.
			//
			if (deviceExtension->NextLowerDriver->Characteristics & FILE_REMOVABLE_MEDIA) {

				DeviceObject->Characteristics |= FILE_REMOVABLE_MEDIA;
			}

#ifdef IOCTL_INTERFACE
			//
			// If the PreviousPnPState is stopped then we are being stopped temporarily
			// and restarted for resource rebalance. 
			//
			if(Stopped != deviceExtension->PreviousPnPState) {
				//
				// Device is started for the first time.
				//
				FilterCreateControlObject(DeviceObject);
			}
#endif   
/*
			VerifyDriverStack(DeviceObject,0,0);

			status = SecureBurnGetTargetDevicePdo(deviceExtension->NextLowerDriver, &PDO);

			if (NT_SUCCESS (status)) 
			{

				pDevObj = PDO;


				if ((pDevObj !=0) )
				{
					ADebugPrint("Driver: %ws", pDevObj->DriverObject->DriverName.Buffer);

					pDrvObj = pDevObj->DriverObject;
					ADebugPrint("Driver name: %ws\n",pDrvObj->DriverName.Buffer);
					if (pDrvObj->MajorFunction[IRP_MJ_SCSI] != HookedIrpRead)
					{
						OldIrpMjRead = pDrvObj->MajorFunction[IRP_MJ_SCSI];
						if(OldIrpMjRead)
						{
							SortDevObj = pDevObj;
							InterlockedExchange((PLONG)&pDrvObj->MajorFunction[IRP_MJ_SCSI],(LONG)HookedIrpRead);
							ADebugPrint("> InterlockedExchange called.. %x",OldIrpMjRead);
						}
					}
				}
			}
			*/
		}

		Irp->IoStatus.Status = status;
		IoCompleteRequest (Irp, IO_NO_INCREMENT);
		IoReleaseRemoveLock(&deviceExtension->RemoveLock, Irp); 
		return status;

	case IRP_MN_REMOVE_DEVICE:

		//
		// Wait for all outstanding requests to complete
		//
		//		ADebugPrint(("Waiting for outstanding requests\n"));
		IoReleaseRemoveLockAndWait(&deviceExtension->RemoveLock, Irp);

		IoSkipCurrentIrpStackLocation(Irp);

		status = IoCallDriver(deviceExtension->NextLowerDriver, Irp);

		SET_NEW_PNP_STATE(deviceExtension, Deleted);

#ifdef IOCTL_INTERFACE
		FilterDeleteControlObject();
#endif 
		IoDetachDevice(deviceExtension->NextLowerDriver);
		IoDeleteDevice(DeviceObject);
		return status;


	case IRP_MN_QUERY_STOP_DEVICE:
		SET_NEW_PNP_STATE(deviceExtension, StopPending);
		status = STATUS_SUCCESS;
		break;

	case IRP_MN_CANCEL_STOP_DEVICE:

		//
		// Check to see whether you have received cancel-stop
		// without first receiving a query-stop. This could happen if someone
		// above us fails a query-stop and passes down the subsequent
		// cancel-stop.
		//

		if(StopPending == deviceExtension->DevicePnPState)
		{
			//
			// We did receive a query-stop, so restore.
			//
			RESTORE_PREVIOUS_PNP_STATE(deviceExtension);
		}
		status = STATUS_SUCCESS; // We must not fail this IRP.
		break;

	case IRP_MN_STOP_DEVICE:
		SET_NEW_PNP_STATE(deviceExtension, Stopped);
		status = STATUS_SUCCESS;
		break;

	case IRP_MN_QUERY_REMOVE_DEVICE:

		SET_NEW_PNP_STATE(deviceExtension, RemovePending);
		status = STATUS_SUCCESS;
		break;

	case IRP_MN_SURPRISE_REMOVAL:

		SET_NEW_PNP_STATE(deviceExtension, SurpriseRemovePending);
		status = STATUS_SUCCESS;
		break;

	case IRP_MN_CANCEL_REMOVE_DEVICE:

		//
		// Check to see whether you have received cancel-remove
		// without first receiving a query-remove. This could happen if
		// someone above us fails a query-remove and passes down the
		// subsequent cancel-remove.
		//

		if(RemovePending == deviceExtension->DevicePnPState)
		{
			//
			// We did receive a query-remove, so restore.
			//
			RESTORE_PREVIOUS_PNP_STATE(deviceExtension);
		}

		status = STATUS_SUCCESS; // We must not fail this IRP.
		break;

	case IRP_MN_DEVICE_USAGE_NOTIFICATION:

		//
		// On the way down, pagable might become set. Mimic the driver
		// above us. If no one is above us, just set pagable.
		//
		if ((DeviceObject->AttachedDevice == NULL) ||
			(DeviceObject->AttachedDevice->Flags & DO_POWER_PAGABLE)) {

				DeviceObject->Flags |= DO_POWER_PAGABLE;
		}

		IoCopyCurrentIrpStackLocationToNext(Irp);

		IoSetCompletionRoutine(
			Irp,
			FilterDeviceUsageNotificationCompletionRoutine,
			NULL,
			TRUE,
			TRUE,
			TRUE
			);

		return IoCallDriver(deviceExtension->NextLowerDriver, Irp);

	default:
		//
		// If you don't handle any IRP you must leave the
		// status as is.
		//
		status = Irp->IoStatus.Status;

		break;
	}

	//
	// Pass the IRP down and forget it.
	//
	Irp->IoStatus.Status = status;
	IoSkipCurrentIrpStackLocation (Irp);
	status = IoCallDriver (deviceExtension->NextLowerDriver, Irp);
	IoReleaseRemoveLock(&deviceExtension->RemoveLock, Irp); 
	return status;
}


NTSTATUS
FilterStartCompletionRoutineScsi(
								 IN PDEVICE_OBJECT   DeviceObject,
								 IN PIRP             Irp,
								 IN PVOID            Context
								 )
								 /*++
								 Routine Description:
								 A completion routine for use when calling the lower device objects to
								 which our filter deviceobject is attached.

								 Arguments:

								 DeviceObject - Pointer to deviceobject
								 Irp          - Pointer to a PnP Irp.
								 Context      - NULL
								 Return Value:

								 NT Status is returned.

								 --*/

{
	PSecureBurnContext pSecureBurnRequestContext = ( PSecureBurnContext) Context; 

	PDEVICE_EXTENSION       deviceExtension;

	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

	/*
	if (Irp->PendingReturned) {

	IoMarkIrpPending(Irp);
	}
	*/

	{
		unsigned char  * pData = (unsigned char*) MmGetSystemAddressForMdlSafe(pSecureBurnRequestContext->pMdl,LowPagePriority );
		if (pData)
		{
			PPrimVolDesc	pPvd = (PPrimVolDesc) pData;

			if( (MmIsAddressValid(&pData[0])) && ( MmIsAddressValid(&pData[2047]) ) )
			{
				//				ADebugPrint(("Scsi PVD: %s  \n",&pPvd->application_use[0]));

				if (!memcmp( &pPvd->application_use[0], "IsoCreator",10  ))
				{
					//					ADebugPrint(("Signature found \n"));
					deviceExtension->SectorTrigger	=	* ((ULONG*) &pPvd->application_use[41]);
					deviceExtension->DestroyRead	=	DESTROY_READ_RIGHT_DISC_NO_TRIGGER;
					//					ADebugPrint(("Setting Destroy read to DESTROY_READ_RIGHT_DISC_NO_TRIGGER and SectorTrigger %d \n", deviceExtension->SectorTrigger));
				}
				else
				{
					//					ADebugPrint(("Signature not found \n"));
					//					ADebugPrint(("Setting Destroy read to DESTROY_READ_WRONG_DISC and SectorTrigger -1 \n"));

					deviceExtension->DestroyRead	= DESTROY_READ_WRONG_DISC;
					deviceExtension->SectorTrigger	= 0xFFFFFFFF;

				}
			} 
		}

	}


	MmUnlockPages(pSecureBurnRequestContext->pMdl);
	IoFreeMdl(pSecureBurnRequestContext->pMdl);
	ExFreePool(pSecureBurnRequestContext);

	return STATUS_CONTINUE_COMPLETION;

	/*
	PKEVENT             event = (PKEVENT)Context;
	UNREFERENCED_PARAMETER (DeviceObject);
	UNREFERENCED_PARAMETER (Irp);


	//
	// If the lower driver didn't return STATUS_PENDING, we don't need to 
	// set the event because we won't be waiting on it. 
	// This optimization avoids grabbing the dispatcher lock, and improves perf.
	//
	if (Irp->PendingReturned == TRUE) {
	KeSetEvent (event, IO_NO_INCREMENT, FALSE);
	}

	//
	// The dispatch routine will have to call IoCompleteRequest
	//

	return STATUS_MORE_PROCESSING_REQUIRED;
	*/

}

NTSTATUS
FilterStartCompletionRoutine(
							 IN PDEVICE_OBJECT   DeviceObject,
							 IN PIRP             Irp,
							 IN PVOID            Context
							 )
							 /*++
							 Routine Description:
							 A completion routine for use when calling the lower device objects to
							 which our filter deviceobject is attached.

							 Arguments:

							 DeviceObject - Pointer to deviceobject
							 Irp          - Pointer to a PnP Irp.
							 Context      - NULL
							 Return Value:

							 NT Status is returned.

							 --*/

{
	PKEVENT             event = (PKEVENT)Context;

	UNREFERENCED_PARAMETER (DeviceObject);

	//
	// If the lower driver didn't return STATUS_PENDING, we don't need to 
	// set the event because we won't be waiting on it. 
	// This optimization avoids grabbing the dispatcher lock, and improves perf.
	//
	if (Irp->PendingReturned == TRUE) {
		KeSetEvent (event, IO_NO_INCREMENT, FALSE);
	}

	//
	// The dispatch routine will have to call IoCompleteRequest
	//

	return STATUS_MORE_PROCESSING_REQUIRED;

}





NTSTATUS
FilterDeviceUsageNotificationCompletionRoutine(
	IN PDEVICE_OBJECT   DeviceObject,
	IN PIRP             Irp,
	IN PVOID            Context
	)
	/*++
	Routine Description:
	A completion routine for use when calling the lower device objects to
	which our filter deviceobject is attached.

	Arguments:

	DeviceObject - Pointer to deviceobject
	Irp          - Pointer to a PnP Irp.
	Context      - NULL
	Return Value:

	NT Status is returned.

	--*/

{
	PDEVICE_EXTENSION       deviceExtension;

	UNREFERENCED_PARAMETER(Context);

	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;


	if (Irp->PendingReturned) {

		IoMarkIrpPending(Irp);
	}

	//
	// On the way up, pagable might become clear. Mimic the driver below us.
	//
	if (!(deviceExtension->NextLowerDriver->Flags & DO_POWER_PAGABLE)) {

		DeviceObject->Flags &= ~DO_POWER_PAGABLE;
	}

	IoReleaseRemoveLock(&deviceExtension->RemoveLock, Irp); 

	return STATUS_CONTINUE_COMPLETION;

}

NTSTATUS
FilterDispatchPower(
					IN PDEVICE_OBJECT    DeviceObject,
					IN PIRP              Irp
					)
					/*++

					Routine Description:

					This routine is the dispatch routine for power irps.

					Arguments:

					DeviceObject - Pointer to the device object.

					Irp - Pointer to the request packet.

					Return Value:

					NT Status code
					--*/
{
	PDEVICE_EXTENSION   deviceExtension;
	NTSTATUS    status;
	//	ADebugPrint (("FilterDispatchPower\n"));
	deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
	status = IoAcquireRemoveLock (&deviceExtension->RemoveLock, Irp);
	if (!NT_SUCCESS (status)) { // may be device is being removed.
		Irp->IoStatus.Status = status;
		PoStartNextPowerIrp(Irp);
		IoCompleteRequest (Irp, IO_NO_INCREMENT);
		return status;
	}

	PoStartNextPowerIrp(Irp);
	IoSkipCurrentIrpStackLocation(Irp);
	status = PoCallDriver(deviceExtension->NextLowerDriver, Irp);
	IoReleaseRemoveLock(&deviceExtension->RemoveLock, Irp); 
	return status;
}



VOID
FilterUnload(
			 IN PDRIVER_OBJECT DriverObject
			 )
			 /*++

			 Routine Description:

			 Free all the allocated resources in DriverEntry, etc.

			 Arguments:

			 DriverObject - pointer to a driver object.

			 Return Value:

			 VOID.

			 --*/
{
	PAGED_CODE ();

	//
	// The device object(s) should be NULL now
	// (since we unload, all the devices objects associated with this
	// driver must be deleted.
	//
	ASSERT(DriverObject->DeviceObject == NULL);

	//
	// We should not be unloaded until all the devices we control
	// have been removed from our queue.
	//
	//	ADebugPrint (("unload\n"));

	return;
}

#ifdef IOCTL_INTERFACE
NTSTATUS
FilterCreateControlObject(
						  IN PDEVICE_OBJECT    DeviceObject
						  )
{
	UNICODE_STRING      ntDeviceName;
	UNICODE_STRING      symbolicLinkName;
	PCONTROL_DEVICE_EXTENSION   deviceExtension;
	NTSTATUS status = STATUS_UNSUCCESSFUL;
	UNICODE_STRING  sddlString;    

	PAGED_CODE();    

	//
	// Using unsafe function so that the IRQL remains at PASSIVE_LEVEL.
	// IoCreateDeviceSecure & IoCreateSymbolicLink must be called at
	// PASSIVE_LEVEL.
	//
	ExAcquireFastMutexUnsafe(&ControlMutex);

	//
	// If this is a first instance of the device, then create a controlobject
	// and register dispatch points to handle ioctls.
	//
	if(1 == ++InstanceCount)
	{

		//
		// Initialize the unicode strings
		//

		RtlInitUnicodeString(&ntDeviceName, NTDEVICE_NAME_STRING);
		RtlInitUnicodeString(&symbolicLinkName, SYMBOLIC_NAME_STRING);

		//
		// Initialize a security descriptor string. Refer to SDDL docs in the SDK
		// for more info.
		//
		RtlInitUnicodeString( &sddlString, L"D:P(A;;GA;;;SY)(A;;GA;;;BA)");

		//
		// Create a named deviceobject so that applications or drivers
		// can directly talk to us without going throuhg the entire stack.
		// This call could fail if there are not enough resources or
		// another deviceobject of same name exists (name collision).
		// Let us use the new IoCreateDeviceSecure and specify a security
		// descriptor (SD) that allows only System and Admin groups to access the 
		// control device. Let us also specify a unique guid to allow administrators 
		// to change the SD if he desires to do so without changing the driver. 
		// The SD will be stored in 
		// HKLM\SYSTEM\CCSet\Control\Class\<GUID>\Properties\Security.
		// An admin can override the SD specified in the below call by modifying
		// the registry.
		//

		status = IoCreateDevice( DeviceObject->DriverObject,				// Our Driver Object
			sizeof( CONTROL_DEVICE_EXTENSION ),      // Size of state information
			&ntDeviceName,                           // Device name "\Device\PCD"
			FILE_DEVICE_UNKNOWN,                     // Device type >von mir festgelegt<
			0,                                       // Device characteristics
			FALSE,                                   // Exclusive device
			&ControlDeviceObject );                  // Returned ptr to Device Object

		/*		status = IoCreateDeviceSecure(DeviceObject->DriverObject,
		sizeof(CONTROL_DEVICE_EXTENSION),
		&ntDeviceName,
		FILE_DEVICE_UNKNOWN,
		FILE_DEVICE_SECURE_OPEN,
		FALSE, 
		&sddlString,
		(LPCGUID)&GUID_SD_FILTER_CONTROL_OBJECT,
		&ControlDeviceObject);
		*/

		if (NT_SUCCESS( status )) {

			ControlDeviceObject->Flags |= DO_BUFFERED_IO;

			status = IoCreateSymbolicLink( &symbolicLinkName, &ntDeviceName );

			if ( !NT_SUCCESS( status )) {
				IoDeleteDevice(ControlDeviceObject);
				//				ADebugPrint(("IoCreateSymbolicLink failed %x\n", status));
				goto End;
			}

			deviceExtension = ControlDeviceObject->DeviceExtension;
			deviceExtension->Type = DEVICE_TYPE_CDO;
			deviceExtension->ControlData = NULL;
			deviceExtension->Deleted = FALSE;
			ControlDeviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

		}else {
			//			ADebugPrint(("IoCreateDevice failed %x\n", status));
		}
	}

End:

	ExReleaseFastMutexUnsafe(&ControlMutex); 
	return status;
}

VOID
FilterDeleteControlObject(
						  )
{
	UNICODE_STRING      symbolicLinkName;
	PCONTROL_DEVICE_EXTENSION   deviceExtension;

	PAGED_CODE();    

	ExAcquireFastMutexUnsafe (&ControlMutex);

	//
	// If this is the last instance of the device then delete the controlobject
	// and symbolic link to enable the pnp manager to unload the driver.
	//

	if(!(--InstanceCount) && ControlDeviceObject)
	{
		RtlInitUnicodeString(&symbolicLinkName, SYMBOLIC_NAME_STRING);
		deviceExtension = ControlDeviceObject->DeviceExtension;
		deviceExtension->Deleted = TRUE;
		IoDeleteSymbolicLink(&symbolicLinkName);
		IoDeleteDevice(ControlDeviceObject);
		ControlDeviceObject = NULL;
	}

	ExReleaseFastMutexUnsafe (&ControlMutex); 

}


NTSTATUS
FilterDispatchIo(
				 IN PDEVICE_OBJECT    DeviceObject,
				 IN PIRP              Irp
				 )
				 /*++

				 Routine Description:

				 This routine is the dispatch routine for non passthru irps.
				 We will check the input device object to see if the request
				 is meant for the control device object. If it is, we will
				 handle and complete the IRP, if not, we will pass it down to 
				 the lower driver.

				 Arguments:

				 DeviceObject - Pointer to the device object.

				 Irp - Pointer to the request packet.

				 Return Value:

				 NT Status code
				 --*/
{
	PIO_STACK_LOCATION  irpStack;
	NTSTATUS            status;
	PCONTROL_DEVICE_EXTENSION   deviceExtension;
	PCOMMON_DEVICE_DATA commonData;
	int					StackLocations;
	ULONG				SizeOfInformation;
	ULONG				BufferSize;
	ULONG						VerificationStatus;
	PSTACK_VERIFICATION_DATA pIoctlBuffer;

	STACK_VERIFICATION_DATA StackVerificationData;

	StackVerificationData.EverythingOk = 1;

	PAGED_CODE();


	commonData = (PCOMMON_DEVICE_DATA)DeviceObject->DeviceExtension;


	//
	// Please note that this is a common dispatch point for controlobject and
	// filter deviceobject attached to the pnp stack. 
	//
	if(commonData->Type == DEVICE_TYPE_FIDO) 
	{
		//
		// We will just  the request down as we are not interested in handling
		// requests that come on the PnP stack.
		//
		return FilterPassIoctl(DeviceObject,Irp);
		//return FilterPass(DeviceObject, Irp);    
	}

	ASSERT(commonData->Type == DEVICE_TYPE_CDO);

	deviceExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

	//
	// Else this is targeted at our control deviceobject so let's handle it.
	// Here we will handle the IOCTl requests that come from the app.
	// We don't have to worry about acquiring remlocks for I/Os that come 
	// on our control object because the I/O manager takes reference on our 
	// deviceobject when it initiates a request to our device and that keeps
	// our driver from unloading when we have pending I/Os. But we still
	// have to watch out for a scenario where another driver can send 
	// requests to our deviceobject directly without opening an handle.
	//
	if(!deviceExtension->Deleted) { //if not deleted
		status = STATUS_SUCCESS;
		Irp->IoStatus.Information = 0;
		irpStack = IoGetCurrentIrpStackLocation (Irp);

		switch (irpStack->MajorFunction) {
			case IRP_MJ_CREATE:
				//				ADebugPrint(("Create \n"));
				break;

			case IRP_MJ_CLOSE:
				//				ADebugPrint(("Close \n"));
				break;

			case IRP_MJ_CLEANUP:
				//				ADebugPrint(("Cleanup \n"));
				break;

			case  IRP_MJ_DEVICE_CONTROL:
								ADebugPrint(("DeviceIoControl %d\n",irpStack->Parameters.DeviceIoControl.IoControlCode));
				switch (irpStack->Parameters.DeviceIoControl.IoControlCode) {
					//
					//case IOCTL_CUSTOM_CODE: 
					//




			case IOCTL_GET_DRIVER_INFORMATION:
				BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
				pIoctlBuffer = Irp->AssociatedIrp.SystemBuffer;

				if	((pIoctlBuffer != NULL)
					|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength> sizeof(STACK_VERIFICATION_DATA))
					|| ( BufferSize>sizeof(STACK_VERIFICATION_DATA)))

				{
					PDEVICE_OBJECT CurrentObj = DeviceObject->DriverObject->DeviceObject;
					while (CurrentObj)
					{
						commonData = (PCOMMON_DEVICE_DATA)CurrentObj->DeviceExtension;

						if (( commonData->Type == DEVICE_TYPE_FIDO)
							&& (pIoctlBuffer->DriveLetter == DeviceObjectToDriveLetter(CurrentObj)))
						{
							VerifyDriverStack(CurrentObj,&VerificationStatus,&pIoctlBuffer->Reason[0]);
							pIoctlBuffer->EverythingOk = VerificationStatus;
							break;
						}
						CurrentObj = CurrentObj->NextDevice;
					}

					Irp->IoStatus.Information = sizeof(STACK_VERIFICATION_DATA);
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;
				}
				break;




			case IOCTL_SBP_VERIFY_DRIVER_STACK:
				BufferSize = irpStack->Parameters.DeviceIoControl.InputBufferLength;
				pIoctlBuffer = Irp->AssociatedIrp.SystemBuffer;

				if	((pIoctlBuffer != NULL)
					|| (irpStack->Parameters.DeviceIoControl.OutputBufferLength>sizeof(STACK_VERIFICATION_DATA))
					|| ( BufferSize>sizeof(STACK_VERIFICATION_DATA)))

				{
					PDEVICE_OBJECT CurrentObj = DeviceObject->DriverObject->DeviceObject;
					while (CurrentObj)
					{
						commonData = (PCOMMON_DEVICE_DATA)CurrentObj->DeviceExtension;

						if (( commonData->Type == DEVICE_TYPE_FIDO)
							&& (pIoctlBuffer->DriveLetter == DeviceObjectToDriveLetter(CurrentObj)))
						{
							VerifyDriverStack(CurrentObj,&VerificationStatus,0);
							StackVerificationData.EverythingOk = VerificationStatus;
							break;
						}
						CurrentObj = CurrentObj->NextDevice;
					}

					memcpy( Irp->AssociatedIrp.SystemBuffer, &StackVerificationData,sizeof(STACK_VERIFICATION_DATA));
					Irp->IoStatus.Information = sizeof(STACK_VERIFICATION_DATA);
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;
				}
				break;
			case IOCTL_SBP_GET_DRIVER_STACK:	
				//				ADebugPrint(("IOCTL_SBP_GET_DRIVER_STACK\n"));

				StackLocations = min(max(g_NoStackLocs, 0),10);

				if ((StackLocations> 0) &&(irpStack->Parameters.DeviceIoControl.OutputBufferLength > StackLocations * sizeof(MyStackLocation)))
				{

					memcpy( Irp->AssociatedIrp.SystemBuffer, &g_LastStackLocations[0], StackLocations * sizeof(MyStackLocation));
				}
				else
				{
					status = STATUS_INVALID_PARAMETER;
				}
				break;
			case IOCTL_SBP_GET_LOADED_DRIVERS:
				//				ADebugPrint(("IOCTL_SBP_GET_LOADED_DRIVERS\n"));
				status = GetLoadedDrivers(Irp->AssociatedIrp.SystemBuffer, 
					irpStack->Parameters.DeviceIoControl.OutputBufferLength,
					&SizeOfInformation);
				Irp->IoStatus.Information = SizeOfInformation;
				break;
				
			case IOCTL_SBP_ENABLE_DISABLE:
				status =EnableDisableDriver(Irp->AssociatedIrp.SystemBuffer, 
					irpStack->Parameters.DeviceIoControl.OutputBufferLength,
					&SizeOfInformation);
				Irp->IoStatus.Information = SizeOfInformation;
				break;

			case IOCTL_SBP_GET_SET_DRM_STATUS:
				status = GetSetLicenseStatus(Irp->AssociatedIrp.SystemBuffer, 
					irpStack->Parameters.DeviceIoControl.OutputBufferLength,
					&SizeOfInformation);
				Irp->IoStatus.Information = SizeOfInformation;
				break;	
			
			case IOCTL_SCSI_EPSON:
			case IOCTL_SCSI_RIMAGE:
			case IOCTL_SCSI_PASS_THROUGH:
			case IOCTL_SCSI_PASS_THROUGH_DIRECT:
				//				ADebugPrint(("IOCTLCODE Found\n"));
			default:
				status = STATUS_INVALID_PARAMETER;
				break;
				}
			default:
				break;
		}
	} else {
		ASSERTMSG(FALSE, "Requests being sent to a dead device\n");
		status = STATUS_DEVICE_REMOVED;
	}
	Irp->IoStatus.Status = status;
	IoCompleteRequest (Irp, IO_NO_INCREMENT);
	return status;
}

#endif 

#if DBG

PCHAR
PnPMinorFunctionString (
						UCHAR MinorFunction
						)
{
	switch (MinorFunction)
	{
	case IRP_MN_START_DEVICE:
		return "IRP_MN_START_DEVICE";
	case IRP_MN_QUERY_REMOVE_DEVICE:
		return "IRP_MN_QUERY_REMOVE_DEVICE";
	case IRP_MN_REMOVE_DEVICE:
		return "IRP_MN_REMOVE_DEVICE";
	case IRP_MN_CANCEL_REMOVE_DEVICE:
		return "IRP_MN_CANCEL_REMOVE_DEVICE";
	case IRP_MN_STOP_DEVICE:
		return "IRP_MN_STOP_DEVICE";
	case IRP_MN_QUERY_STOP_DEVICE:
		return "IRP_MN_QUERY_STOP_DEVICE";
	case IRP_MN_CANCEL_STOP_DEVICE:
		return "IRP_MN_CANCEL_STOP_DEVICE";
	case IRP_MN_QUERY_DEVICE_RELATIONS:
		return "IRP_MN_QUERY_DEVICE_RELATIONS";
	case IRP_MN_QUERY_INTERFACE:
		return "IRP_MN_QUERY_INTERFACE";
	case IRP_MN_QUERY_CAPABILITIES:
		return "IRP_MN_QUERY_CAPABILITIES";
	case IRP_MN_QUERY_RESOURCES:
		return "IRP_MN_QUERY_RESOURCES";
	case IRP_MN_QUERY_RESOURCE_REQUIREMENTS:
		return "IRP_MN_QUERY_RESOURCE_REQUIREMENTS";
	case IRP_MN_QUERY_DEVICE_TEXT:
		return "IRP_MN_QUERY_DEVICE_TEXT";
	case IRP_MN_FILTER_RESOURCE_REQUIREMENTS:
		return "IRP_MN_FILTER_RESOURCE_REQUIREMENTS";
	case IRP_MN_READ_CONFIG:
		return "IRP_MN_READ_CONFIG";
	case IRP_MN_WRITE_CONFIG:
		return "IRP_MN_WRITE_CONFIG";
	case IRP_MN_EJECT:
		return "IRP_MN_EJECT";
	case IRP_MN_SET_LOCK:
		return "IRP_MN_SET_LOCK";
	case IRP_MN_QUERY_ID:
		return "IRP_MN_QUERY_ID";
	case IRP_MN_QUERY_PNP_DEVICE_STATE:
		return "IRP_MN_QUERY_PNP_DEVICE_STATE";
	case IRP_MN_QUERY_BUS_INFORMATION:
		return "IRP_MN_QUERY_BUS_INFORMATION";
	case IRP_MN_DEVICE_USAGE_NOTIFICATION:
		return "IRP_MN_DEVICE_USAGE_NOTIFICATION";
	case IRP_MN_SURPRISE_REMOVAL:
		return "IRP_MN_SURPRISE_REMOVAL";

	default:
		return "unknown_pnp_irp";
	}
}

#endif



char
DeviceObjectToDriveLetter(PDEVICE_OBJECT pDevObj)
{
	NTSTATUS		ntStatus;
	UNICODE_STRING	uszDeviceName;
	char			DriveLetter;
	char			Result  = 0;
	STRING			ntNameString;
	UNICODE_STRING	ntUnicodeString;
	WCHAR			filename[]				= L"\\DosDevices\\C:";

	PDEVICE_OBJECT	pLetterDeviceObject;
	PDEVICE_OBJECT	pDriveTop				= NULL;
	PFILE_OBJECT	letterFileObject		= NULL;


	PAGED_CODE();																									
	pDriveTop =	IoGetAttachedDeviceReference ( pDevObj );
	if (pDriveTop == NULL)
	{
		return 0;
	}

	for ( DriveLetter = 'A'; DriveLetter <=  'Z'; DriveLetter++)
	{

		filename[12] = DriveLetter;																							
		RtlInitUnicodeString( &uszDeviceName, filename	);																

		ntStatus = IoGetDeviceObjectPointer(
			&uszDeviceName,
			FILE_READ_ATTRIBUTES,
			&letterFileObject,
			&pLetterDeviceObject);	

		if(	NT_SUCCESS(ntStatus))
		{
			ObDereferenceObject( letterFileObject );

			if ( pLetterDeviceObject == pDriveTop)
			{
				Result = DriveLetter;
				break;
			}
		}
	}
	ObDereferenceObject(pDriveTop);
	return Result;

}



PDEVICE_EXTENSION GetRoxioDeviceExtension(UCHAR Host, UCHAR Drive)
{
	PDEVICE_EXTENSION deviceExtension;
	if ((Host > 99) || (Drive > 99))
	{
		// Host or drive out of range
		return 0;
	}
	if (!RoxioDeviceExtension[Host][Drive])
	{
		// No Device Object present for this device - create one
		
		deviceExtension = (PDEVICE_EXTENSION) ExAllocatePoolWithTag(NonPagedPool, sizeof(DEVICE_EXTENSION), 1234);
		if (deviceExtension)
		{
			// Got one - initialize it..
			memset(deviceExtension, 0, sizeof(DEVICE_EXTENSION));
			deviceExtension->NextLowerDriver 				= 0;
			deviceExtension->SectorTrigger					= 0xFFFFFFFF;
			deviceExtension->DestroyRead					= DESTROY_READ_WRONG_DISC;
			deviceExtension->Type							= DEVICE_TYPE_FIDO;
			deviceExtension->LastWriteCommand 				= 0xBADF00D;				// Last write command recieved by us		
			deviceExtension->AssumedNextWriteCommand 		= 0xBADF00D;		// Assumed next write command to be recieved.
			deviceExtension->AddressToPatch			 		= 0xBADF00D;
			deviceExtension->NextLowerDriver 				= 0;
			RoxioDeviceExtension[Host][Drive] 				= deviceExtension;
		}

		return deviceExtension;
	
	}
	else
	{
		return RoxioDeviceExtension[Host][Drive];
	}
}

PDEVICE_EXTENSION GetRoxioDeviceExtension_Prassi(UCHAR Host, UCHAR Drive)
{
	PDEVICE_EXTENSION deviceExtension;
	if ((Host > 99) || (Drive > 99))
	{
		// Host or drive out of range
		return 0;
	}
	if (!PrassiDeviceExtension[Host][Drive])
	{
		// No Device Object present for this device - create one
		
		deviceExtension = (PDEVICE_EXTENSION) ExAllocatePoolWithTag(NonPagedPool, sizeof(DEVICE_EXTENSION), 1234);
		if (deviceExtension)
		{
			// Got one - initialize it..
			memset(deviceExtension, 0, sizeof(DEVICE_EXTENSION));
			deviceExtension->NextLowerDriver 					= 0;
			deviceExtension->SectorTrigger						= 0xFFFFFFFF;
			deviceExtension->DestroyRead							= DESTROY_READ_WRONG_DISC;
			deviceExtension->Type											= DEVICE_TYPE_FIDO;
			deviceExtension->LastWriteCommand 				= 0xBADF00D;				// Last write command recieved by us		
			deviceExtension->AssumedNextWriteCommand 	= 0xBADF00D;		// Assumed next write command to be recieved.
			deviceExtension->AddressToPatch			 			= 0xBADF00D;
			deviceExtension->NextLowerDriver 					= 0;
			RoxioDeviceExtension[Host][Drive] 				= deviceExtension;
		}

		return deviceExtension;
	
	}
	else
	{
		return PrassiDeviceExtension[Host][Drive];
	}
}

NTSTATUS IoCompletionRoutine(IN PDEVICE_OBJECT DeviceObject, 
							 IN PIRP Irp, 
							 IN PVOID Context)
{
	PVOID OutputBuffer;
  DWORD NumOutputBuffers;
	PIO_COMPLETION_ROUTINE p_compRoutine;
	DWORD i;
	PROXIO_SCSI_COMMAND pRoxio;

	// Connection status values:
	// 0 = Invisible
	// 1 = CLOSED
	// 2 = LISTENING
	// 3 = SYN_SENT
	// 4 = SYN_RECEIVED
	// 5 = ESTABLISHED
	// 6 = FIN_WAIT_1
	// 7 = FIN_WAIT_2
	// 8 = CLOSE_WAIT
	// 9 = CLOSING
	// ...

	OutputBuffer = Irp->UserBuffer;
	p_compRoutine = ((PREQINFO)Context)->OldCompletion;

	
	pRoxio = ((PREQINFO)Context)->RoxioCommand;
	pRoxio->Buffer = ((PREQINFO)Context)->SwappedBuffer;
	
	ExFreePool(Context);


	if ((Irp->StackCount > (ULONG)1) && (p_compRoutine != NULL))
	{
		return (p_compRoutine)(DeviceObject, Irp, NULL);
	}
	else
	{
		return Irp->IoStatus.Status;
	}
}

NTSTATUS HookedDeviceControl_Roxio(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	return HookedDeviceControl(DeviceObject,Irp , ROXIO_CONTROL);
}
NTSTATUS HookedDeviceControl_Prassi	(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp)
{
	return HookedDeviceControl(DeviceObject,Irp, PRASSI_CONTROL);
}

NTSTATUS HookedDeviceControl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, int Control)
{
	NTSTATUS 								ntStatus;
	PIO_STACK_LOCATION			irpSp;
	ULONG BufferLen;
	UCHAR * inputBuffer;
	ULONG i;
	UCHAR * Cdb;
	UCHAR * DataBuffer;
	UCHAR * pPvd;
	PROXIO_SCSI_COMMAND pRoxio;
	PDEVICE_EXTENSION deviceExtension;
	PUCHAR pTmp;
	PUCHAR pOrgBuffer;
	
	ntStatus = 0xDEADBEEF;
	if (g_Disabled != 0)
	{
			if (Control == PRASSI_CONTROL)
			{
				ntStatus = OldIrpMjDeviceControl_Prassi(DeviceObject, Irp);
			}
			else
			{
				ntStatus = OldIrpMjDeviceControl(DeviceObject, Irp);
			}
		 return ntStatus;
	}
	//ADebugPrint(("The current IRP is at %x %x\n", Irp, DeviceObject));

	
	irpSp = IoGetCurrentIrpStackLocation( Irp );
	
	ADebugPrint(("IOCTL hooked: %x ", irpSp->Parameters.DeviceIoControl.IoControlCode));
	
	if ((irpSp->Parameters.DeviceIoControl.IoControlCode == 0x22203f) ||
		 ( irpSp->Parameters.DeviceIoControl.IoControlCode == 0x22200B))
	{
			inputBuffer = (UCHAR *) irpSp->Parameters.DeviceIoControl.Type3InputBuffer;
			pRoxio =   (PROXIO_SCSI_COMMAND) inputBuffer;

			if (Control == PRASSI_CONTROL)
			{
				deviceExtension = GetRoxioDeviceExtension_Prassi(pRoxio->Host, pRoxio->Drive);
			}
			else
			{
				deviceExtension = GetRoxioDeviceExtension(pRoxio->Host, pRoxio->Drive);
			}
			
			if (deviceExtension)
			{
				Cdb = (PUCHAR) &pRoxio->Cdb[0];
				BufferLen =  irpSp->Parameters.DeviceIoControl.InputBufferLength;
				DataBuffer = pRoxio->Buffer;
				try 
				{                                                                                                                    
				    ProbeForWrite(pRoxio->Buffer, pRoxio->BufferLen , 1);
				  //  DoScsiCommand(&pspt->Cdb[0], DeviceObject,  deviceExtension, (PUCHAR)  ( pspt + pspt->DataBufferOffset),-1, 0,0);
						
						pTmp = DoScsiCommand(Cdb, 0, deviceExtension, 
											pRoxio->Buffer,pRoxio->BufferLen, 1, 1);
															
						if (pTmp)
						{
							// Call our completion routine if IRP successful
							
							pOrgBuffer = pRoxio->Buffer;
							pRoxio->Buffer = pTmp;
							
								if (Control == PRASSI_CONTROL)
								{
									ntStatus = OldIrpMjDeviceControl_Prassi(DeviceObject, Irp);
								}
								else
								{
									ntStatus = OldIrpMjDeviceControl(DeviceObject, Irp);
							}
							//ntStatus = OldIrpMjDeviceControl(DeviceObject, Irp);
							pRoxio->Buffer = pOrgBuffer;
										
							
						}
					
		
				}                                                                  
				except (EXCEPTION_EXECUTE_HANDLER) 
				{
				} 
				
				if (ntStatus != 0xDEADBEEF)
				{
					return ntStatus;
				}
				
			}
		 
			                                                                                  
				                                                                                                                          
	}		
		
		       
			                                        	         
	//ADebugPrint(("Before: %x ", irpSp->Context));
	
	if (Control == PRASSI_CONTROL)
	{
		ntStatus = OldIrpMjDeviceControl_Prassi(DeviceObject, Irp);
	}
	else
	{
		ntStatus = OldIrpMjDeviceControl(DeviceObject, Irp);
	}
	//ADebugPrint(("After: %x  %x", irpSp->Context, ntStatus));
  return  ntStatus;
}

NTSTATUS InstallTCPDriverHook()
{
#ifdef _WIN64
	return STATUS_SUCCESS;
#else  
	NTSTATUS       ntStatus;
      
	UNICODE_STRING deviceTCPUnicodeString;
	WCHAR deviceTCPNameBuffer[]  = L"\\Device\\PxHelperDevice0";
	pFile_tcp  = NULL;
	pDev_tcp   = NULL;
	pDrv_tcpip = NULL;
	OldIrpMjDeviceControl = NULL;   

	RtlInitUnicodeString (&deviceTCPUnicodeString, deviceTCPNameBuffer);
	ntStatus = IoGetDeviceObjectPointer(&deviceTCPUnicodeString, FILE_READ_DATA, &pFile_tcp, &pDev_tcp);
	if(!NT_SUCCESS(ntStatus)) 
		return ntStatus;
	pDrv_tcpip = pDev_tcp->DriverObject;

	OldIrpMjDeviceControl = pDrv_tcpip->MajorFunction[IRP_MJ_DEVICE_CONTROL]; 
	if (OldIrpMjDeviceControl)
		InterlockedExchange ((PLONG)&pDrv_tcpip->MajorFunction[IRP_MJ_DEVICE_CONTROL], (LONG)HookedDeviceControl_Roxio);
	ADebugPrint(("Hoookey hooky Dev: %x\n", pDev_tcp));
	return STATUS_SUCCESS;
#endif
}


NTSTATUS InstallTCPDriverHook_Prassi()
{
#ifdef _WIN64
	return STATUS_SUCCESS;
#else
  NTSTATUS       ntStatus;
  
	UNICODE_STRING deviceTCPUnicodeString;
	WCHAR deviceTCPNameBuffer[]  = L"\\Device\\PzWDMDevice0";
  pFile_tcp_Prassi  = NULL;
	pDev_tcp_Prassi   = NULL;
	pDrv_tcpip_Prassi = NULL;
	OldIrpMjDeviceControl_Prassi = NULL;   

	RtlInitUnicodeString (&deviceTCPUnicodeString, deviceTCPNameBuffer);
	ntStatus = IoGetDeviceObjectPointer(&deviceTCPUnicodeString, FILE_READ_DATA, &pFile_tcp_Prassi, &pDev_tcp_Prassi);
	if(!NT_SUCCESS(ntStatus)) 
		return ntStatus;
	pDrv_tcpip_Prassi = pDev_tcp_Prassi->DriverObject;

	OldIrpMjDeviceControl_Prassi = pDrv_tcpip_Prassi->MajorFunction[IRP_MJ_DEVICE_CONTROL]; 
	
	
	
		if (OldIrpMjDeviceControl_Prassi)
			InterlockedExchange ((PLONG)&pDrv_tcpip_Prassi->MajorFunction[IRP_MJ_DEVICE_CONTROL], (LONG)HookedDeviceControl_Prassi);

	ADebugPrint(("Hoookey hooky Prassi Dev: %x\n", pDev_tcp_Prassi));
#endif
	return STATUS_SUCCESS;
}
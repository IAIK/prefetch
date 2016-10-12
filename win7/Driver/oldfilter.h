
// Build 64Bit: Windows Vista and Windows Server Longhorn (Free Build)
/*++

Copyright (c) Microsoft Corporation.  All rights reserved.

THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
PURPOSE.

Module Name:

filter.c

Abstract:

This module shows how to a write a generic filter driver.

Environment:

Kernel mode

Revision History:

Eliyas Yakub Oct 29 1998

Fixed bugs - March 15, 2001

Added Ioctl interface - Aug 16, 2001

Updated to use IoCreateDeviceSecure function - Sep 17, 2002

Updated to use RemLocks - Oct 29, 2002
/*++

Copyright (c) Microsoft Corporation.  All rights reserved.

    THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY
    KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR
    PURPOSE.

Module Name:

    filter.h

Abstract:

    Contains structure definitions and function prototypes for filter driver.

Environment:

    Kernel mode

Revision History:

    Eliyas Yakub Oct 29 1998

--*/
#include <ntddk.h>
#include <wdmsec.h> // for IoCreateDeviceSecure
#include <initguid.h>
#include <srb.h>

// old - {41966169-3FD7-4392-AFE4-E6A9D0A92C72}  - generated using guidgen.exe
// new - {ABBA4D03-2393-4f16-833E-41CFE81ED7E6}

#define IOCTL_SCSI_RIMAGE 0x4D03C
#define IOCTL_SCSI_EPSON  0x22BB0

DEFINE_GUID (GUID_SD_FILTER_CONTROL_OBJECT,
        0xABBA4D03, 0x2393, 0x4f16, 0x83, 0x3E, 0x41, 0xCF, 0xE8, 0x1E, 0xD7, 0xE6);

#define IOCTL_INTERFACE

#define	IOCTL_SBP_BASE					0xA012
#define	IOCTL_SBP_GET_DRIVER_STACK		CTL_CODE(IOCTL_SBP_BASE, 0x803,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_SBP_GET_LOADED_DRIVERS	CTL_CODE(IOCTL_SBP_BASE, 0x804,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define	IOCTL_SBP_VERIFY_DRIVER_STACK	CTL_CODE(IOCTL_SBP_BASE, 0x805,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_GET_DRIVER_INFORMATION    CTL_CODE(IOCTL_SBP_BASE, 0x806,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_GET_SET_DRM_STATUS    CTL_CODE(IOCTL_SBP_BASE, 0x807,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_ENABLE_DISABLE		CTL_CODE(IOCTL_SBP_BASE, 0x808,	METHOD_BUFFERED, FILE_ANY_ACCESS)


typedef NTSTATUS (*OLDIRPMJREAD)(IN PDEVICE_OBJECT, IN PIRP);
OLDIRPMJREAD OldIrpMjRead;


typedef	struct _MODULE_INFORMATION
{

	PUCHAR ul_Address;
	ULONG ul_Size;
	ULONG ul_Unknown2;
	ULONG ulIndex;
	ULONG ulUnknown3;

#if defined(_X86_) 
	char szName[260+1];
#else
	char szName[264+1];
#endif
} MODULE_INFORMATION ,* PMODULE_INFORMATION;

typedef struct _MODULE_INFO_BASE
{	
	ULONG	ul_NumberOfModules;
	ULONG	Unknown1;
	ULONG	Unknown2;
	
#if defined(_X86_) 
	//;
#else
	ULONG	Unknown3[3];
#endif
	MODULE_INFORMATION miModules[1];
} MODULE_INFO_BASE, *PMODULE_INFO_BASE;

typedef struct _STACK_VERIFICATION_DATA
{
	ULONG	EverythingOk;
	char	DriveLetter;
	char	Reason[265];
} STACK_VERIFICATION_DATA, *PSTACK_VERIFICATION_DATA;

//
// GUID definition are required to be outside of header inclusion pragma to avoid
// error during precompiled headers.
//

//#define DBG 1

#if !defined(_FILTER_H_)
#define _FILTER_H_

#define DRIVERNAME "filter.sys: "

#if BUS_LOWER

#undef DRIVERNAME
#define DRIVERNAME "BFdoLwr.sys: "

#endif

#if BUS_UPPER

#undef DRIVERNAME
#define DRIVERNAME "BFdoUpr.sys: "

#endif

#if DEVICE_LOWER

#undef DRIVERNAME
#define DRIVERNAME "DevLower.sys: "

#endif

#if DEVICE_UPPER

#undef DRIVERNAME
#define DRIVERNAME "DevUpper.sys: "

#endif

#if CLASS_LOWER

#undef DRIVERNAME
#define DRIVERNAME "SBP001.SYS "

#endif

#if CLASS_UPPER

#undef DRIVERNAME
#define DRIVERNAME "ClsUpper.sys: "

#endif

#if DBG
#define ADebugPrint(_x_);
//#define ADebugPrint(_x_)\
//              DbgPrint _x_;

#define TRAP() DbgBreakPoint()

#else
                  
#define ADebugPrint(_x_);
//#define ADebugPrint(_x_)          DbgPrint _x_;

/*define ADebugPrint(_x_)\
                if (Evil < 2000)\
                	{ Evil++; \
               DbgPrint _x_; \
             }
*/           

#define TRAP()
#endif



#define		WRITE_ENCRYPTION_ENABLE		1
#define		WRITE_ENCRYPTION_DISABLE	2
#define		VERIFY_DRIVER_STACK			3
#ifndef  STATUS_CONTINUE_COMPLETION //required to build driver in Win2K and XP build environment
//
// This value should be returned from completion routines to continue
// completing the IRP upwards. Otherwise, STATUS_MORE_PROCESSING_REQUIRED
// should be returned.
//
#define STATUS_CONTINUE_COMPLETION      STATUS_SUCCESS

#endif

#define POOL_TAG   'liFT'

#define FlagOn(F,SF) (    \
    (((F) & (SF)) != 0) \
)

#define DESTROY_READ_WRONG_DISC				0
#define DESTROY_READ_RIGHT_DISC_NO_TRIGGER	1
#define DESTROY_READ_TRIGGERED				2

//	================================================================================================
//	CallGate Stuff
//	================================================================================================


#define	IDX_CG_RtlInitString					0
#define	IDX_CG_RtlAnsiStringToUnicodeString		1
#define	IDX_CG_ExFreePool						2	
#define	IDX_CG_IoFreeMdl						3
#define	IDX_CG_MmUnlockPages					4
#define	IDX_CG_IofCallDriver					5
#define	IDX_CG_IoAllocateIrp					6
#define	IDX_CG_ExAllocatePool					7
#define	IDX_CG_KeWaitForSingleObject			8
#define	IDX_CG_IoBuildSynchronousFsdRequest		9
#define	IDX_CG_KeInitializeEvent				10
#define	IDX_CG_RtlFreeUnicodeString				11
#define	IDX_CG_IoGetAttachedDevice				12
#define	IDX_CG_IoFreeIrp						13
#define	IDX_CG_IoGetDeviceObjectPointer			14
#define	IDX_CG_RtlInitUnicodeString				15
#define	IDX_CG_KeSetAffinityThread				16
#define	IDX_CG_PsGetCurrentProcessId			17
#define	IDX_CG_IoDeleteSymbolicLink				18
#define	IDX_CG_IoDeleteDevice					19
#define	IDX_CG_IoCreateSymbolicLink				20
#define	IDX_CG_IofCompleteRequest				21
#define	IDX_CG_KeSetEvent						22
#define	IDX_CG_ReleaseQueue						23
#define	IDX_CG_ObfDereferenceObject				24
#define	IDX_CG_IoCreateDevice					25
#define IDX_CG_ZwQuerySystemInformation			26
#define IDX_CG_ZwCreateFile						27

/*
typedef	enum	_CALLGATE_INDEX
{
	CG_RtlInitString					= 0,		//	VOID:		PSTRING, PCSZ
	CG_RtlAnsiStringToUnicodeString		= 1,		//	NTSTATUS:	PUNICODE_STRING, PANSI_STRING, BOOLEAN
	CG_ExFreePool						= 2,
	CG_IoFreeMdl						= 3,
	CG_MmUnlockPages					= 4,
	CG_IofCallDriver					= 5,
	CG_IoAllocateIrp					= 6,
	CG_ExAllocatePool					= 7,
	CG_KeWaitForSingleObject			= 8,
	CG_IoBuildSynchronousFsdRequest		= 9,
	CG_KeInitializeEvent				= 10,
	CG_RtlFreeUnicodeString				= 11,
	CG_IoGetAttachedDevice				= 12,
	CG_IoFreeIrp						= 13,
	CG_IoGetDeviceObjectPointer			= 14,
	CG_RtlInitUnicodeString				= 15,
	CG_KeSetAffinityThread				= 16,
	CG_PsGetCurrentProcessId			= 17,
	CG_IoDeleteSymbolicLink				= 18,
	CG_IoDeleteDevice					= 19,
	CG_IoCreateSymbolicLink				= 20,
	CG_IofCompleteRequest				= 21,
	CG_KeSetEvent						= 22,
	CG_ReleaseQueue						= 23,
	CG_ObfDereferenceObject				= 24,
	CG_IoCreateDevice					= 25,
	CG_ZwQuerySystemInformation			= 26,
	CG_ZwCreateFile						= 27

}	CALLGATE_INDEX;
*/

typedef	enum	_CALLGATE_INDEX
{
	CG_RtlInitString					= 9928919,	//	VOID:		PSTRING, PCSZ
	CG_RtlAnsiStringToUnicodeString		= 8781727,	//	NTSTATUS:	PUNICODE_STRING, PANSI_STRING, BOOLEAN
	CG_ExFreePool						= 8273818,
	CG_IoFreeMdl						= 2938019,
	CG_MmUnlockPages					= 2838192,
	CG_IofCallDriver					= 8828191,
	CG_IoAllocateIrp					= 2728821,
	CG_ExAllocatePool					= 8811919,
	CG_KeWaitForSingleObject			= 8278727,
	CG_IoBuildSynchronousFsdRequest		= 4722728,
	CG_KeInitializeEvent				= 7726161,
	CG_RtlFreeUnicodeString				= 8782717,
	CG_IoGetAttachedDevice				= 1828818,
	CG_IoFreeIrp						= 2934919,
	CG_IoGetDeviceObjectPointer			= 4999393,
	CG_RtlInitUnicodeString				= 7577757,
	CG_KeSetAffinityThread				= 9199911,
	CG_PsGetCurrentProcessId			= 3471787,
	CG_IoDeleteSymbolicLink				= 8989828,
	CG_IoDeleteDevice					= 1233133,
	CG_IoCreateSymbolicLink				= 1222432,
	CG_IofCompleteRequest				= 9182828,
	CG_KeSetEvent						= 7772818,
	CG_ReleaseQueue						= 8882381,
	CG_ObfDereferenceObject				= 2311226,
	CG_IoCreateDevice					= 1991029,
	CG_ZwQuerySystemInformation			= 2318381,
	CG_ZwCreateFile						= 8888282,

}	CALLGATE_INDEX;

typedef enum _SYSTEM_INFORMATION_CLASS {
    SystemBasicInformation,		// 0
    SystemProcessorInformation,		// 1
    SystemSubsystemsInformation,	// 2
    SystemTimeInformation,		// 3		// fixed 0x20
    SystemPathInformation,		// 4		// now available via SharedUserData
    SystemProcessInformation,		// 5		// 0x88
    SystemServiceInformation,		// 6		// ?? (KeServiceDescriptorTableShadow)
    SystemConfigurationInformation,	// 7		// fixed 0x18
    SystemProcessorXxxInformation,	// 8		// 0x30
    SystemGlobalFlagInformation,	// 9
    SystemXxxx1Information,		// 10		// STATUS_NOT_IMPLEMENTED
    SystemLoadedModuleInformation,	// 11		// ??
    SystemLockInformation,		// 12		// 0x28
    SystemStackTraceInformation,	// 13		// 0x5C
    SystemPagedPoolInformation,		// 14		// 0x1C
    SystemNonPagedPoolInformation,	// 15		// 0x1C
    SystemHandleInformation,		// 16		// 0x14
    SystemObjectInformation,		// 17		// 0x38
    SystemPageFileInformation,		// 18		// 0x18
    SystemInstemulInformation,		// 19		// 0x88
    SystemInformationReserved20,	// 20
    SystemCacheInformation,		// 21
    SystemPoolTagInformation,		// 22		// 0x20
    SystemProcessorsInformation,	// 23		// 0x18 * KeNumberProcessors
    SystemDpcInformation,		// 24
    SystemMemoryUsageInformation,	// 25		// 0x14
    SystemLoadSystemImage,		// 26		// fixed 0x18, can be only set by kernel
    SystemUnloadSystemImage,		// 27		// fixed 0x04, can be only set by kernel
    SystemTimeXxxInformation,		// 28		// fixed 0x0C, can be set (fixed 0x08)
    SystemMemoryXxxUsageInformation,	// 29		// 0x14
    SystemSetEventIdInformation,	// 30		// ???
    SystemQueryEventIdInformation,	// 31		// ???
    SystemCrashDumpInformation,		// 32		// 0x4
    SystemProcessorsXxxInformation,	// 33		// 0x10
    SystemCrashDumpStateInformation,	// 34		// 0x4
    SystemDebugInformation,		// 35
    SystemThreadXxxInformation,		// 35		// 0x30
    SystemQueryRegistryQuotaInformation,// 36		// 0xC
    SystemSetRegistryQuotaInformation,	// 37		// fixed 0x0C, can only be set
    SystemSystemImageXxxx,		// 38		// fixed 0x08, can only be set
    SystemSetPrioritySeparation,	// 39		// fixed 0x04, can only be set
    SystemXxxx2Information,		// 40		// STATUS_NOT_IMPLEMENTED
    SystemInformationReserved41,	// 41
    SystemInformationReserved42,	// 42
    SystemTimeZoneInformation,		// 43		// 0xAC
    SystemLookasideInformation,		// 44		// ???
    SystemMaximumInformation		// 45
} SYSTEM_INFORMATION_CLASS, *PSYSTEM_INFORMATION_CLASS;

//
// These are the states Filter transition to upon
// receiving a specific PnP Irp. Refer to the PnP Device States
// diagram in DDK documentation for better understanding.
//


typedef enum _DEVICE_PNP_STATE {

    NotStarted = 0,         // Not started yet
    Started,                // Device has received the START_DEVICE IRP
    StopPending,            // Device has received the QUERY_STOP IRP
    Stopped,                // Device has received the STOP_DEVICE IRP
    RemovePending,          // Device has received the QUERY_REMOVE IRP
    SurpriseRemovePending,  // Device has received the SURPRISE_REMOVE IRP
    Deleted                 // Device has received the REMOVE_DEVICE IRP

} DEVICE_PNP_STATE;

#define INITIALIZE_PNP_STATE(_Data_)    \
        (_Data_)->DevicePnPState =  NotStarted;\
        (_Data_)->PreviousPnPState = NotStarted;

#define SET_NEW_PNP_STATE(_Data_, _state_) \
        (_Data_)->PreviousPnPState =  (_Data_)->DevicePnPState;\
        (_Data_)->DevicePnPState = (_state_);

#define RESTORE_PREVIOUS_PNP_STATE(_Data_)   \
        (_Data_)->DevicePnPState =   (_Data_)->PreviousPnPState;\

typedef enum _DEVICE_TYPE {

    DEVICE_TYPE_INVALID = 0,         // Invalid Type;
    DEVICE_TYPE_FIDO,                // Device is a filter device.
    DEVICE_TYPE_CDO,                 // Device is a control device.

} DEVICE_TYPE;

//
// A common header for the device extensions of the Filter and control
// device objects
//

typedef struct _COMMON_DEVICE_DATA
{
    
    DEVICE_TYPE Type;
    
} COMMON_DEVICE_DATA, *PCOMMON_DEVICE_DATA;


typedef struct _DEVICE_EXTENSION
{
    COMMON_DEVICE_DATA;

    //
    // A back pointer to the device object.
    //

    PDEVICE_OBJECT  Self;

    //
    // The top of the stack before this filter was added.
    //

    PDEVICE_OBJECT  NextLowerDriver;

    //
    // current PnP state of the device
    //

    DEVICE_PNP_STATE  DevicePnPState;

    //
    // Remembers the previous pnp state
    //

    DEVICE_PNP_STATE    PreviousPnPState;

    //
    // Removelock to track IRPs so that device can be removed and 
    // the driver can be unloaded safely.
    //
    IO_REMOVE_LOCK RemoveLock; 

	PDRIVER_DISPATCH	PreviousScsiHandler;
	//
	// Trigger sector for Read protection....
	//

    UCHAR PFIBuffer[2048];
	ULONG	ProtectVersion; 
	ULONG DiscSize;
	ULONG	SectorForPFI;
	ULONG	SectorTrigger;			
	ULONG	DestroyRead;
	ULONG	LastWriteCommand;				// Last write command recieved by us		
	ULONG	AssumedNextWriteCommand;		// Assumed next write command to be recieved.
	ULONG	AddressToPatch;					// Address with demo mode in it.
	ULONG StartDecryptionAddress;  
	ULONG	EndSector;
	ULONG	DecreaseLicense;
	ULONG		DataLen;
	PUCHAR  DataBuffer;
	ULONG		WriteInProgress;
	unsigned char WriteKey[16];
	UCHAR EncryptedProjectId[16];

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

PDEVICE_OBJECT
pcdDriveLetterToCdRomDevice(
PDEVICE_OBJECT pDriveDeviceObject
);

PCHAR
PnPMinorFunctionString (
    UCHAR MinorFunction
);

NTSTATUS
FilterAddDevice(
    IN PDRIVER_OBJECT DriverObject,
    IN PDEVICE_OBJECT PhysicalDeviceObject
    );


NTSTATUS
FilterDispatchPnp (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
FilterDispatchPower(
    IN PDEVICE_OBJECT    DeviceObject,
    IN PIRP              Irp
    );

VOID
FilterUnload(
    IN PDRIVER_OBJECT DriverObject
    );

NTSTATUS
FilterPassScsi (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
FilterPassIoctl (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
FilterPass (
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
DriverEntry(
    IN PDRIVER_OBJECT  DriverObject,
    IN PUNICODE_STRING RegistryPath
    );

NTSTATUS
FilterStartCompletionRoutineScsi(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
	);

NTSTATUS
FilterStartCompletionRoutine(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
    );

NTSTATUS
FilterDeviceUsageNotificationCompletionRoutine(
    IN PDEVICE_OBJECT   DeviceObject,
    IN PIRP             Irp,
    IN PVOID            Context
    );

#define SENSE_BUFFER_SIZE 18

typedef	struct	_PCD_SCSI_Struct
{
	ULONG				Id;
	KEVENT				event;
	PIRP				irp,
						MasterIrp;
	LARGE_INTEGER		StartTime;
	LARGE_INTEGER		EndTime;
	SCSI_REQUEST_BLOCK	srb;
	IO_STATUS_BLOCK		ioStatus;
	LARGE_INTEGER		StartOffset;
	UCHAR				Sense[SENSE_BUFFER_SIZE];


} PCD_SCSI_Struct, *PPCD_SCSI_Struct;


typedef char TimeAndDate[17];


#pragma pack(1)

typedef struct _SECURE_BURN_COMMAND
{
	unsigned char UpperStructSize;
	unsigned char LowerStructSize;
	unsigned char Zero[2];
	unsigned char Signature[16];
	unsigned char Command;
	unsigned char AdditionalData[400];
} SECURE_BURN_COMMAND, *PSECURE_BURN_COMMAND;




typedef	struct _Date
{
	unsigned char		year;
	unsigned char		month;
	unsigned char		day;
	unsigned char		hour;
	unsigned char		minute;
	unsigned char		second;
	char				tz;
} Date, *PDate;

typedef	struct _DirectoryRecord
{
	unsigned char		length;				//1		0
	unsigned char		ext_attr_length;	//2		1
	unsigned long		extent_loc_i;		//6		2
	unsigned long		extent_loc_m;		//10	6
	unsigned long		data_length_i;		// 14	10
	unsigned long		data_length_m;		//18	14
	Date				Date;				// 25	18
	unsigned char		flags;				// 26
	unsigned char		file_unit_size;		// 27
	unsigned char		gap_size;			// 28
	unsigned short		sequence_i;			// 30
	unsigned short		sequence_m;			// 32
	unsigned char		file_id_length;		// 33
	char				file_id[1];
} DirectoryRecord, *PDirectoryRecord;


typedef	struct _PrimVolDesc
{
	unsigned char		type;					// 0
	char				id[5];					// 1
	unsigned char		version;				// 6
	char				pad1;					// 7
	char				system_id[32];			// 8
	char				volume_id[32];			// 40
	char				pad2[8];				// 72
	unsigned long		space_size_i;			// 80
	unsigned long		space_size_m;			// 84
	char				pad3[32];				// 88
	unsigned short		set_size_i;				// 120
	unsigned short		set_size_m;				// 122
	unsigned short		sequence_i;				// 124
	unsigned short		sequence_m;				// 126
	unsigned short		block_size_i;			// 128
	unsigned short		block_size_m;			// 130
	unsigned long		path_size_i;			// 132
	unsigned long		path_size_m;			// 136
	unsigned long		l_table;				// 140
	unsigned long		opt_l_table;			// 144
	unsigned long		m_table;				// 148
	unsigned long		opt_m_table;			// 152
	DirectoryRecord		root;					// 156
	char				volume_set_id[128];
	char				publisher_id[128];
	char				data_preparer[128];
	char				application_id[128];
	char				copyright[37];
	char				abstract_file_id[37];
	char				bibliographic_id[37];
	TimeAndDate			vol_creation;
	TimeAndDate			vol_modification;
	TimeAndDate			vol_expiration;
	TimeAndDate			vol_effective;
	unsigned char		file_structure_version;
	char				pad4;
	char				application_use[512];
	char				reserved[653];
} PrimVolDesc, *PPrimVolDesc;
#pragma pack()

typedef struct _SecureBurnContext
{
	PMDL pMdl;
	PSCSI_REQUEST_BLOCK pSrb;
}SecureBurnContext,*PSecureBurnContext;

#ifdef IOCTL_INTERFACE

#define NTDEVICE_NAME_STRING      L"\\Device\\secburn"
#define SYMBOLIC_NAME_STRING      L"\\DosDevices\\secburn"

typedef struct _CONTROL_DEVICE_EXTENSION {

    COMMON_DEVICE_DATA;

    ULONG   Deleted; // False if the deviceobject is valid, TRUE if it's deleted
    
    PVOID   ControlData; // Store your control data here
    
} CONTROL_DEVICE_EXTENSION, *PCONTROL_DEVICE_EXTENSION;

NTSTATUS
FilterCreateControlObject(
    IN PDEVICE_OBJECT    DeviceObject
);

VOID
FilterDeleteControlObject(
    );

NTSTATUS
FilterDispatchIo(
    IN PDEVICE_OBJECT    DeviceObject,
    IN PIRP              Irp
    );


NTSTATUS HookedDeviceControl(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp, int Control);
NTSTATUS HookedDeviceControl_Roxio(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS HookedDeviceControl_Prassi(IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp);
NTSTATUS InstallTCPDriverHook();
NTSTATUS InstallTCPDriverHook_Prassi();
PFILE_OBJECT pFile_tcp;
PDEVICE_OBJECT pDev_tcp;
PDRIVER_OBJECT pDrv_tcpip;

PFILE_OBJECT 		pFile_tcp_Prassi;
PDEVICE_OBJECT 	pDev_tcp_Prassi;
PDRIVER_OBJECT 	pDrv_tcpip_Prassi;


typedef NTSTATUS (*OLDIRPMJDEVICECONTROL)(IN PDEVICE_OBJECT, IN PIRP);
OLDIRPMJDEVICECONTROL OldIrpMjDeviceControl;
OLDIRPMJDEVICECONTROL OldIrpMjDeviceControl_Prassi;


#define PRASSI_CONTROL 0
#define ROXIO_CONTROL 1


PDEVICE_EXTENSION RoxioDeviceExtension[60][60] = {0};
PDEVICE_EXTENSION PrassiDeviceExtension[60][60] = {0};
 #pragma pack(1)
 

typedef struct _ROXIO_SCSI_COMMAND
{
	UCHAR Host;
	UCHAR Drive;
	UCHAR Unknown11[5];
	UCHAR CdbLen;
	UCHAR Cdb[16];
	UCHAR Unknown2;
	ULONG	BufferLen;
	UCHAR * Buffer;
	UCHAR Unknown3[20]; 
		
}   ROXIO_SCSI_COMMAND, *PROXIO_SCSI_COMMAND;

#pragma pack()

typedef struct _REQINFO {
	PIO_COMPLETION_ROUTINE OldCompletion;
	unsigned char * SwappedBuffer;
	ROXIO_SCSI_COMMAND* RoxioCommand;
} REQINFO, *PREQINFO;

#endif

#endif



#define IOCTL_INTERFACE

#define	IOCTL_SBP_BASE					0xA013
#define	IOCTL_SBP_START	CTL_CODE(IOCTL_SBP_BASE, 0x805,	METHOD_BUFFERED, FILE_ANY_ACCESS) 
#define IOCTL_SBP_STOP CTL_CODE(IOCTL_SBP_BASE, 0x806,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_LOADCR4  CTL_CODE(IOCTL_SBP_BASE, 0x807,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_GETPHYSICALADDRESS  CTL_CODE(IOCTL_SBP_BASE, 0x808,	METHOD_BUFFERED, FILE_ANY_ACCESS)

#define IOCTL_SBP_GETMSR  CTL_CODE(IOCTL_SBP_BASE, 0x809,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_SETMSR  CTL_CODE(IOCTL_SBP_BASE, 0x80a,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_FLUSH  CTL_CODE(IOCTL_SBP_BASE, 0x80b,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_SBP_ACCESS_MEMORY  CTL_CODE(IOCTL_SBP_BASE, 0x80c,	METHOD_BUFFERED, FILE_ANY_ACCESS)

#define IOCTL_SBP_PREFETCH  CTL_CODE(IOCTL_SBP_BASE, 0x80d,	METHOD_BUFFERED, FILE_ANY_ACCESS)
#pragma pack( push, PREIDT )
typedef struct NT_IDT
{
	unsigned short    wLoOfs;        // Low Word of the ISR's offset
	unsigned short    wSelector;     // ISR's selector....should be 8 under NT
	unsigned short    wFlags;        // Flags...should almost always be 8E00 
	// for 32-bit code on NT.
	unsigned short    wHiOfs;        // Hi Word of ISR's offset

} NT_IDT, *PNT_IDT;

#define DWORD unsigned int
#define WORD unsigned short
#define BYTE unsigned char
#pragma pack(1)
typedef struct tagIDT
{
	WORD	wLimit;
	DWORD	dwBase;
} IDT, *PIDT;

typedef struct tagINT_VECTOR
{
	WORD	wLowOffset;
	WORD	wSelector;
	BYTE	bAccess;
	BYTE	wUnused;
	WORD	wHighOffset;
} INT_VECTOR, *PINT_VECTOR;

#pragma pack()

#define VEC_OFFSET_TO_DWORD( _vec ) \
	_vec.wLowOffset | _vec.wHighOffset << 16

#define DWORD_TO_VEC_OFFSET( _vec, _dword ) \
	_vec.wLowOffset = (WORD)_dword; \
	_vec.wHighOffset = (WORD)( (DWORD)_dword >> 16 );

#define VEC_GET_DPL( _vec ) \
	( _vec.bAccess & 0x60 ) >> 4
#define VEC_SET_DPL( _vec, _value ) \
	_vec.bAccess &= 0x9F; \
	_value << 4; \
	_vec.bAccess |= (BYTE)_value;
#define VEC_IS_PRESENT( _vec ) \
	_vec.bAccess >> 7
#define VEC_SET_PRESENT( _vec ) \
	_vec.bAccess |= 0x80;
#define VEC_GET_TYPE( _vec ) \
	_vec.bAccess & 0xF0
#define SELECTOR_GET_RPL( _sel ) \
	_sel & 0x3
#define SELECT_SET_RPL( _sel, _rpl ) \
	_sel &= 0xFFC; \
	_sel |= (WORD)_rpl;


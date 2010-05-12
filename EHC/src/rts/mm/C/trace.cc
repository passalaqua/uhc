
%%[8
#include "../../rts.h"

extern int nodeDescriptor[];  // generated by compiling the user program; contains size of node for each tag

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace implementation for C backend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
void mm_trace_C_Init( MM_Trace* trace, void* traceSupply, MM_Allocator* allocator, MM_Collector* collector ) 
{
    // printf("mm_trace_C_Init\n");

	// Store the given parameters in the trace object.
	trace->data      = (MM_Trace_Data_Priv*)traceSupply ;
	trace->collector = collector ;
	trace->allocator = allocator ;
}

Bool mm_trace_C_CanTraceObject( MM_Trace* trace, Word obj ) 
{
    return 1;
}


#define FORWARDING_TAG 0xFFFFFFFF

Word mm_trace_C_TraceKnownToBeObject( MM_Trace* trace, Word obj ) 
{
    // This function is responsible for copying an object "from-space" to "to-space".
    // If the object was already copied before, the copy is returnd.
    // If the object was not yet copied, it is done now:
    // - a new object is allocated
    // - its payload is copied
    // - the pointers in the payload a queued for copying as well
    // The original object is overwritten with a forwarding node.
    // The new object is returned.

    // printf("        mm_trace_C_TraceKnownToBeObject obj=%08x\n", obj); fflush(stdout);

    
	Word tag;
	tag=((WPtr)obj)[0];

    // printf("          tag=%d %08x obj=%08x\n", tag, tag, obj); fflush(stdout);


	if (tag==FORWARDING_TAG)
    {
        // The node is a forwarding node, indicating that the node was copied before,
        // so we can just return that and we're done
		obj = ((WPtr)obj)[1];
        // printf("          return obj=%08x\n", obj); fflush(stdout);
		return obj ;
	}

    // Now we are sure that node "obj", with tag "tag", must be copied.

    // Find out the size of the new object to be allocated

    Word descr = nodeDescriptor[tag];
	Word nodeSize    = (  tag==0 
	                   ?  ((WPtr)obj)[1]  // BlackHole has nodesize stored in its payload
	                   :  descr >> 16
	                   );
	Word payloadSize = (descr & 0xFFFF ) >> 1;

    // printf("            tag=%d nodesize=%d plsize=%d\n", tag, nodeSize, payloadSize);

    // Allocate the new object
	MM_Allocator *allocator  =  trace->allocator ;
    WPtr objRepl;
	objRepl = (WPtr)( allocator->alloc( allocator, nodeSize << Word_SizeInBytes_Log, 0 ) ) ;    // alloc wants the size in bytes, so multiply the szWords by the Word_Size

    // printf("            allocated repl=%08x\n", objRepl );


    // Initialize the new object
	Word* fieldTo   =       objRepl;
	Word* fieldFrom = (WPtr)obj;
	for (int sz=payloadSize; sz>=0 ; sz-- ) 
	{
        // printf("            copy word from %08x to %08x sz=%d data=%08x\n", fieldFrom, fieldTo, sz, *fieldFrom );
		*(fieldTo++) = *(fieldFrom++) ;
	}
	
	//Copy a nice pattern in the part of the node that is not yet used.
	//Only necessary for debugging.
	// for (int k=payloadSize+1; k<nodeSize; k++)
	// {   objRepl[k] = 0xA4A4A4A4;
	//      // printf("            copy filler to %08x k=%d\n", objRepl+k, k );
	// }

    // printf("            initialized tag=%d fwd tag at %08x to %08x\n", objRepl[0], ((WPtr)obj), objRepl );

    // Overwrite the original object with a forwarding node, which points to the new object
    // The forwarding node has a special tag

	((WPtr)obj)[0] = FORWARDING_TAG ;

	// The forwarding node has a single-word payload, which is the pointer to the new object
	((WPtr)obj)[1] = (Word)objRepl ;
	

    // Queue the payload of the freshly copied object to be processed as well
    MM_TraceSupply* traceSupply = (MM_TraceSupply*)trace->data;
	traceSupply->pushWork( traceSupply, (Word*)objRepl, payloadSize+1, allocator->lastAllocFragment(allocator) ) ;

    // printf("            pushwork repl=%08x\n", objRepl );
	return (Word)objRepl ;
}

void mm_trace_C_TraceObjectPayload( MM_Trace* trace, Word obj ) 
{
    // printf("        mm_trace_C_TraceObjectPayload obj=%08x\n", obj); fflush(stdout);

	Word tag;
	tag=((WPtr)obj)[0];
    Word descr = nodeDescriptor[tag];

    //printf("          tag=%d %08x\n", tag, tag); fflush(stdout);
    
    if (! (descr&1))
        return;
    
	Word payloadSize = (descr & 0xFFFF ) >> 1;

    // printf("          payloadSize=%d\n", payloadSize); fflush(stdout);

    
	WPtr payload    = ((WPtr)obj)+1 ;
	WPtr payloadEnd = payload + payloadSize ;
	for ( ; payload < payloadEnd ; payload++ )
	{
		*payload = mm_trace_C_TraceKnownToBeObject( trace, *payload ) ;
	}
    
}

Word mm_trace_C_ObjectSize( MM_Trace* trace, Word obj ) 
{
	Word tag;
	tag=((WPtr)obj)[0];
    Word descr = nodeDescriptor[tag];
	Word payloadSize = (descr & 0xFFFF ) >> 1;
	return payloadSize;
}

Bool mm_trace_C_HasTraceableWords( MM_Trace* trace, Word obj ) 
{
	Word tag;
	tag=((WPtr)obj)[0];
    Word descr = nodeDescriptor[tag];
	return descr & 1;
}

Word mm_trace_C_EnsureNoIndirections( MM_Trace* trace, Word obj ) 
{
    return obj;
}

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MM_Trace interface object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
MM_Trace mm_trace_C =
	{ NULL
	, NULL
	, NULL
	, 1              // size of node header in Words
	, sizeof(Word)   // size of node header in bytes
	, &mm_trace_C_Init
	, &mm_trace_C_CanTraceObject
	, &mm_trace_C_TraceKnownToBeObject
	, &mm_trace_C_TraceObjectPayload
	, &mm_trace_C_ObjectSize
	, &mm_trace_C_HasTraceableWords
	, &mm_trace_C_EnsureNoIndirections
	} ;
%%]

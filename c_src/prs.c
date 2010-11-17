/*
	Original code from the PRSutil project.
	http://www.fuzziqersoftware.com/projects.php
*/

#include <string.h>

typedef struct {
    unsigned char bitpos;
    unsigned char* controlbyteptr;
    unsigned char* srcptr_orig;
    unsigned char* dstptr_orig;
    unsigned char* srcptr;
    unsigned char* dstptr; } PRS_COMPRESSOR;

void prs_put_control_bit(PRS_COMPRESSOR* pc,unsigned char bit)
{
    *pc->controlbyteptr = *pc->controlbyteptr >> 1;
    *pc->controlbyteptr |= ((!!bit) << 7);
    pc->bitpos++;
    if (pc->bitpos >= 8)
    {
        pc->bitpos = 0;
        pc->controlbyteptr = pc->dstptr;
        pc->dstptr++;
    }
}

void prs_put_control_bit_nosave(PRS_COMPRESSOR* pc,unsigned char bit)
{
    *pc->controlbyteptr = *pc->controlbyteptr >> 1;
    *pc->controlbyteptr |= ((!!bit) << 7);
    pc->bitpos++;
}

void prs_put_control_save(PRS_COMPRESSOR* pc)
{
    if (pc->bitpos >= 8)
    {
        pc->bitpos = 0;
        pc->controlbyteptr = pc->dstptr;
        pc->dstptr++;
    }
}

void prs_put_static_data(PRS_COMPRESSOR* pc,unsigned char data)
{
    *pc->dstptr = data;
    pc->dstptr++;
}

unsigned char prs_get_static_data(PRS_COMPRESSOR* pc)
{
    unsigned char data = *pc->srcptr;
    pc->srcptr++;
    return data;
}

/* **************************************************** */

void prs_init(PRS_COMPRESSOR* pc,void* src,void* dst)
{
    pc->bitpos = 0;
    pc->srcptr = (unsigned char*)src;
    pc->srcptr_orig = (unsigned char*)src;
    pc->dstptr = (unsigned char*)dst;
    pc->dstptr_orig = (unsigned char*)dst;
    pc->controlbyteptr = pc->dstptr;
    pc->dstptr++;
}

void prs_finish(PRS_COMPRESSOR* pc)
{
    prs_put_control_bit(pc,0);
    prs_put_control_bit(pc,1);
    if (pc->bitpos != 0)
    {
        *pc->controlbyteptr = ((*pc->controlbyteptr << pc->bitpos) >> 8);
    }
    prs_put_static_data(pc,0);
    prs_put_static_data(pc,0);
}

void prs_rawbyte(PRS_COMPRESSOR* pc)
{
    prs_put_control_bit_nosave(pc,1);
    prs_put_static_data(pc,prs_get_static_data(pc));
    prs_put_control_save(pc);
}

void prs_shortcopy(PRS_COMPRESSOR* pc,int offset,unsigned char size)
{
    size -= 2;
    prs_put_control_bit(pc,0);
    prs_put_control_bit(pc,0);
    prs_put_control_bit(pc,(size >> 1) & 1);
    prs_put_control_bit_nosave(pc,size & 1);
    prs_put_static_data(pc,offset & 0xFF);
    prs_put_control_save(pc);
}

void prs_longcopy(PRS_COMPRESSOR* pc,int offset,unsigned char size)
{
    if (size <= 9)
    {
        prs_put_control_bit(pc,0);
        prs_put_control_bit_nosave(pc,1);
        prs_put_static_data(pc,((offset << 3) & 0xF8) | ((size - 2) & 0x07));
        prs_put_static_data(pc,(offset >> 5) & 0xFF);
        prs_put_control_save(pc);
    } else {
        prs_put_control_bit(pc,0);
        prs_put_control_bit_nosave(pc,1);
        prs_put_static_data(pc,(offset << 3) & 0xF8);
        prs_put_static_data(pc,(offset >> 5) & 0xFF);
        prs_put_static_data(pc,size - 1);
        prs_put_control_save(pc);
    }
}

void prs_copy(PRS_COMPRESSOR* pc,int offset,unsigned char size)
{
    if ((offset > -0x100) && (size <= 5))
        prs_shortcopy(pc,offset,size);
    else
        prs_longcopy(pc,offset,size);
    pc->srcptr += size;
}

/* **************************************************** */

unsigned long prs_compress(void* source,void* dest,unsigned long size)
{
    PRS_COMPRESSOR pc;
    int x,y;
    unsigned long xsize;
    int lsoffset,lssize;
    prs_init(&pc,source,dest);
    for (x = 0; x < size; x++)
    {
        lsoffset = lssize = xsize = 0;
        for (y = x - 3; (y > 0) && (y > (x - 0x1FF0)) && (xsize < 255); y--)
        {
            xsize = 3;
            if (!memcmp((void*)((unsigned long)source + y),(void*)((unsigned long)source + x),xsize))
            {
                do xsize++;
                while (!memcmp((void*)((unsigned long)source + y),
                               (void*)((unsigned long)source + x),
                               xsize) &&
                       (xsize < 256) &&
                       ((y + xsize) < x) &&
                       ((x + xsize) <= size)
                );
                xsize--;
                if (xsize > lssize)
                {
                    lsoffset = -(x - y);
                    lssize = xsize;
                }
            }
        }
        if (lssize == 0)
        {
            prs_rawbyte(&pc);
        } else {
            prs_copy(&pc,lsoffset,lssize);
            x += (lssize - 1);
        }
    }
    prs_finish(&pc);
    return pc.dstptr - pc.dstptr_orig;
}

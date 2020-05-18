#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "circular.h"

cbuf_handle_t circular_buf_init(size_t size)
{
	assert(size);
	uint8_t* buffer    = (uint8_t*)      malloc(sizeof(uint8_t) * (size + 1));
	cbuf_handle_t cbuf = (cbuf_handle_t) malloc(sizeof(circular_buf_t));
	cbuf->buffer       = buffer;
	cbuf->max          = size;
	circular_buf_reset(cbuf);
	return cbuf;
}

void circular_buf_reset(cbuf_handle_t cbuf)
{
    assert(cbuf);
    cbuf->head = 0;
    cbuf->tail = 0;
    cbuf->full = 0;
}

void circular_buf_free(cbuf_handle_t cbuf)
{
	assert(cbuf);
	free(cbuf->buffer);
	free(cbuf);
}

int circular_buf_full(cbuf_handle_t cbuf)
{
	assert(cbuf);
    return cbuf->full;
}

int circular_buf_empty(cbuf_handle_t cbuf)
{
	assert(cbuf);
    return (!cbuf->full && (cbuf->head == cbuf->tail));
}

size_t circular_buf_capacity(cbuf_handle_t cbuf)
{
	assert(cbuf);
	return cbuf->max;
}

size_t circular_buf_size(cbuf_handle_t cbuf)
{
	assert(cbuf);
	size_t size = cbuf->max;
	if(!cbuf->full)
	{
		if(cbuf->head >= cbuf->tail)
		{
			size = (cbuf->head - cbuf->tail);
		}
		else
		{
			size = (cbuf->max + cbuf->head - cbuf->tail);
		}
	}
	return size;
}

static void advance_pointer(cbuf_handle_t cbuf)
{
	assert(cbuf);
	// if(cbuf->full) {
	// 	cbuf->tail = (cbuf->tail + 1) % cbuf->max;
	// }

	cbuf->head = (cbuf->head + 1) % cbuf->max;
	cbuf->full = (cbuf->head == cbuf->tail);
}

static void retreat_pointer(cbuf_handle_t cbuf)
{
	assert(cbuf);
	cbuf->full = 0;
	cbuf->tail = (cbuf->tail + 1) % cbuf->max;
}

void circular_buf_put(cbuf_handle_t cbuf, uint8_t data)
{
	assert(cbuf && cbuf->buffer);
    cbuf->buffer[cbuf->head] = data;
    advance_pointer(cbuf);
}

int circular_buf_put2(cbuf_handle_t cbuf, uint8_t data)
{
    int r = -1;
    assert(cbuf && cbuf->buffer);
    if(!circular_buf_full(cbuf))
    {
        cbuf->buffer[cbuf->head] = data;
        advance_pointer(cbuf);
        r = 0;
    }
    return r;
}

int circular_buf_get(cbuf_handle_t cbuf, uint8_t * data)
{
    assert(cbuf && data && cbuf->buffer);
    int r = -1;
    if(!circular_buf_empty(cbuf))
    {
        *data = cbuf->buffer[cbuf->tail];
        retreat_pointer(cbuf);
        r = 0;
    }
    return r;
}

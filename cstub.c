#define CAML_NAME_SPACE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/memory.h>
#include <caml/alloc.h>

#include "cassandra.h"


void print_error(CassFuture* future) {
  const char* message;
  size_t message_length;
  cass_future_error_message(future, &message, &message_length);
  fprintf(stderr, "Error: %.*s\nthis was an error\n", (int)message_length, message);
}

CAMLprim value match_enum(value rc, value future){
	CAMLparam2(rc, future);
	if (rc != CASS_OK) {
		print_error((CassFuture*)future);	
    fprintf(stderr, "\ninside error of match enum\n");
    	CAMLreturn(Val_int(0));//false
  }else{
    CAMLreturn (Val_int(1));//true
  }
 }
 
 CAMLprim value convert_to_ml(value val){
  CAMLparam1(val);
  CAMLreturn(Val_int(val));
 }

 CAMLprim value convert(value val){
 	CAMLparam1(val);
 	CAMLreturn(Int_val(val));
 }

 CAMLprim value convert_to_bool(value val){
  CAMLparam1(val);
  if (val == 1){
    CAMLreturn (Val_int(1));
  }else{
    CAMLreturn(Val_int(0));
  }
 }

CAMLprim value get_string(value val){
  
  CAMLparam1(val);
  CAMLlocal1(var_value);
  const char* text;
  size_t text_length;
  
  cass_value_get_string((const CassValue*)val, &text, &text_length);
  
  int c = 0;
  int length = (int)text_length;
  
  char sub [length];
  
  while (c < length) {
      sub[c] = text[c];
      c++;
  }
  sub[c] = '\0';
  var_value = caml_copy_string(sub);
  CAMLreturn(var_value);
 }

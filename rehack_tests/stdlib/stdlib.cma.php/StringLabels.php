<?hh // strict
// Copyright 2004-present Facebook. All Rights Reserved.

/**
 * @generated
 *
 */
namespace Rehack;

final class StringLabels {
  <<__Override, __Memoize>>
  public static function requireModule() : Vector<dynamic> {
    $joo_global_object = \Rehack\GlobalObject::get() as dynamic;
    
    $runtime = $joo_global_object->jsoo_runtime;
    $String =  String_::requireModule ();
    $make = $String[1];
    $init = $String[2];
    $copy = $String[3];
    $sub = $String[4];
    $fill = $String[5];
    $blit = $String[6];
    $concat = $String[7];
    $iter = $String[8];
    $iteri = $String[9];
    $map = $String[10];
    $mapi = $String[11];
    $trim = $String[12];
    $escaped = $String[13];
    $index = $String[14];
    $index_opt = $String[15];
    $rindex = $String[16];
    $rindex_opt = $String[17];
    $index_from = $String[18];
    $index_from_opt = $String[19];
    $rindex_from = $String[20];
    $rindex_from_opt = $String[21];
    $contains = $String[22];
    $contains_from = $String[23];
    $rcontains_from = $String[24];
    $uppercase = $String[25];
    $lowercase = $String[26];
    $capitalize = $String[27];
    $uncapitalize = $String[28];
    $uppercase_ascii = $String[29];
    $lowercase_ascii = $String[30];
    $capitalize_ascii = $String[31];
    $uncapitalize_ascii = $String[32];
    $compare = $String[33];
    $equal = $String[34];
    $split_on_char = $String[35];
    $StringLabels = Vector{
      0,
      $make,
      $init,
      $copy,
      $sub,
      $fill,
      $blit,
      $concat,
      $iter,
      $iteri,
      $map,
      $mapi,
      $trim,
      $escaped,
      $index,
      $index_opt,
      $rindex,
      $rindex_opt,
      $index_from,
      $index_from_opt,
      $rindex_from,
      $rindex_from_opt,
      $contains,
      $contains_from,
      $rcontains_from,
      $uppercase,
      $lowercase,
      $capitalize,
      $uncapitalize,
      $uppercase_ascii,
      $lowercase_ascii,
      $capitalize_ascii,
      $uncapitalize_ascii,
      $compare,
      $equal,
      $split_on_char
    } as dynamic;
    
     return ($StringLabels);

  }

}
/* Hashing disabled */

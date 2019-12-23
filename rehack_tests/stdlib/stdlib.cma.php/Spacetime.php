<?hh // strict
// Copyright 2004-present Facebook. All Rights Reserved.

/**
 * @generated
 *
 */
namespace Rehack;

final class Spacetime {
  <<__Override, __Memoize>>
  public static function requireModule() : Vector<dynamic> {
    $joo_global_object = \Rehack\GlobalObject::get() as dynamic;
    
    $runtime = $joo_global_object->jsoo_runtime;
    $call1 = $runtime["caml_call1"];
    $string = $runtime["caml_new_string"];
    $caml_spacetime_enabled = $runtime["caml_spacetime_enabled"];
    $caml_spacetime_only_works_for_native_code = $runtime[
       "caml_spacetime_only_works_for_native_code"
     ];
    $cst_Series_is_closed__0 = $string("Series is closed");
    $cst_Series_is_closed = $string("Series is closed");
    $Pervasives =  Pervasives::get ();
    $enabled = $caml_spacetime_enabled(0);
    $if_spacetime_enabled = (dynamic $f) ==> {
      return $enabled ? $call1($f, 0) : (0);
    };
    $create = (dynamic $path) ==> {
      if ($caml_spacetime_enabled(0)) {
        $channel = $call1($Pervasives[48], $path);
        $t = Vector{0, $channel, 0};
        $caml_spacetime_only_works_for_native_code($channel);
        return $t;
      }
      return Vector{0, $Pervasives[27], 1};
    };
    $save_event = (dynamic $time, dynamic $t, dynamic $event_name) ==> {
      return $if_spacetime_enabled(
        (dynamic $param) ==> {
          return $caml_spacetime_only_works_for_native_code(
            $time,
            $t[1],
            $event_name
          );
        }
      );
    };
    $save_and_close = (dynamic $time, dynamic $t) ==> {
      return $if_spacetime_enabled(
        (dynamic $param) ==> {
          if ($t[2]) {$call1($Pervasives[2], $cst_Series_is_closed);}
          $caml_spacetime_only_works_for_native_code($time, $t[1]);
          $call1($Pervasives[64], $t[1]);
          $t[2] = 1;
          return 0;
        }
      );
    };
    $Series = Vector{0, $create, $save_event, $save_and_close};
    $take = (dynamic $time, dynamic $param) ==> {
      $channel = $param[1];
      $closed = $param[2];
      return $if_spacetime_enabled(
        (dynamic $param) ==> {
          if ($closed) {$call1($Pervasives[2], $cst_Series_is_closed__0);}
          $runtime["caml_gc_minor"](0);
          return $caml_spacetime_only_works_for_native_code($time, $channel);
        }
      );
    };
    $Snapshot = Vector{0, $take};
    $save_event_for_automatic_snapshots = (dynamic $event_name) ==> {
      return $if_spacetime_enabled(
        (dynamic $param) ==> {
          return $caml_spacetime_only_works_for_native_code($event_name);
        }
      );
    };
    $Spacetime = Vector{
      0,
      $enabled,
      $Series,
      $Snapshot,
      $save_event_for_automatic_snapshots
    };
    
     return ($Spacetime);

  }
  public static function enabled(): dynamic {
    return static::callRehackFunction(static::requireModule()[1], varray[]);
  }
  public static function Series(): dynamic {
    return static::callRehackFunction(static::requireModule()[2], varray[]);
  }
  public static function Snapshot(): dynamic {
    return static::callRehackFunction(static::requireModule()[3], varray[]);
  }
  public static function save_event_for_automatic_snapshots(dynamic $event_name): dynamic {
    return static::callRehackFunction(static::requireModule()[4], varray[$event_name]);
  }

}
/* Hashing disabled */

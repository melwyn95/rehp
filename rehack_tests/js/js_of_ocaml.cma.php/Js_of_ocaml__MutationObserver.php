<?hh
// Copyright 2004-present Facebook. All Rights Reserved.

/**
 * Js_of_ocaml__MutationObserver.php
 */

namespace Rehack;

final class Js_of_ocaml__MutationObserver {
  <<__Memoize>>
  public static function get() {
    $global_object = \Rehack\GlobalObject::get();
    $runtime = \Rehack\Runtime::get();
    /*
     * Soon, these will replace the `global_data->ModuleName`
     * pattern in the load() function.
     */
    $Array_ = Array_::get();
    $Js_of_ocaml__Js = Js_of_ocaml__Js::get();
    Js_of_ocaml__MutationObserver::load($global_object);
    $memoized = $runtime->caml_get_global_data()->Js_of_ocaml__MutationObserver;
    return $memoized;
  }

  /**
   * Performs module load operation. May have side effects.
   */
  private static function load($joo_global_object) {
    

    $runtime = $joo_global_object->jsoo_runtime;
    $caml_arity_test = $runtime["caml_arity_test"];
    $caml_get_public_method = $runtime["caml_get_public_method"];
    $call1 = function(dynamic $f, dynamic $a0) use ($caml_arity_test,$runtime) {
      return $caml_arity_test($f) === 1
        ? $f($a0)
        : ($runtime["caml_call_gen"]($f, varray[$a0]));
    };
    $global_data = $runtime["caml_get_global_data"]();
    $Array = $global_data["Array_"];
    $Js_of_ocaml_Js = $global_data["Js_of_ocaml__Js"];
    $empty_mutation_observer_init = function(dynamic $param) {
      return (object)darray[];
    };
    $pG = function(dynamic $x) use ($call1,$caml_get_public_method) {
      return $call1($caml_get_public_method($x, -412262690, 258), $x);
    };
    $pH = $Js_of_ocaml_Js[50][1];
    $mutationObserver = (function(dynamic $t0, dynamic $param) {return $t0->MutationObserver;
     })($pH, $pG);
    $is_supported = function(dynamic $param) use ($Js_of_ocaml_Js,$call1,$mutationObserver) {
      return $call1($Js_of_ocaml_Js[6][5], $mutationObserver);
    };
    $observe = function
    (dynamic $node, dynamic $f, dynamic $child_list, dynamic $attributes, dynamic $character_data, dynamic $subtree, dynamic $attribute_old_value, dynamic $character_data_old_value, dynamic $attribute_filter, dynamic $param) use ($Array,$call1,$caml_get_public_method,$empty_mutation_observer_init,$mutationObserver,$runtime) {
      $opt_iter = function(dynamic $x, dynamic $f) use ($call1) {
        if ($x) {$x__0 = $x[1];return $call1($f, $x__0);}
        return 0;
      };
      $pI = 0;
      $pJ = $runtime["caml_js_wrap_callback"]($f);
      $obs = (function(dynamic $t19, dynamic $t18, dynamic $param) {return new $t19($t18);
       })($mutationObserver, $pJ, $pI);
      $cfg = $empty_mutation_observer_init(0);
      $opt_iter(
        $child_list,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pS = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, -749670374, 259), $x);
          };
          return (function(dynamic $t17, dynamic $t16, dynamic $param) {$t17->childList = $t16;return 0;
           })($cfg, $v, $pS);
        }
      );
      $opt_iter(
        $attributes,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pR = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, 393324759, 260), $x);
          };
          return (function(dynamic $t15, dynamic $t14, dynamic $param) {$t15->attributes = $t14;return 0;
           })($cfg, $v, $pR);
        }
      );
      $opt_iter(
        $character_data,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pQ = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, 995092083, 261), $x);
          };
          return (function(dynamic $t13, dynamic $t12, dynamic $param) {$t13->characterData = $t12;return 0;
           })($cfg, $v, $pQ);
        }
      );
      $opt_iter(
        $subtree,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pP = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, 808321758, 262), $x);
          };
          return (function(dynamic $t11, dynamic $t10, dynamic $param) {$t11->subtree = $t10;return 0;
           })($cfg, $v, $pP);
        }
      );
      $opt_iter(
        $attribute_old_value,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pO = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, 226312582, 263), $x);
          };
          return (function(dynamic $t9, dynamic $t8, dynamic $param) {$t9->attributeOldValue = $t8;return 0;
           })($cfg, $v, $pO);
        }
      );
      $opt_iter(
        $character_data_old_value,
        function(dynamic $v) use ($call1,$caml_get_public_method,$cfg) {
          $pN = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, 994928349, 264), $x);
          };
          return (function(dynamic $t7, dynamic $t6, dynamic $param) {$t7->characterDataOldValue = $t6;return 0;
           })($cfg, $v, $pN);
        }
      );
      $opt_iter(
        $attribute_filter,
        function(dynamic $l) use ($Array,$call1,$caml_get_public_method,$cfg,$runtime) {
          $pL = function(dynamic $x) use ($call1,$caml_get_public_method) {
            return $call1($caml_get_public_method($x, -116981516, 265), $x);
          };
          $pM = $runtime["caml_js_from_array"]($call1($Array[12], $l));
          return (function(dynamic $t5, dynamic $t4, dynamic $param) {$t5->attributeFilter = $t4;return 0;
           })($cfg, $pM, $pL);
        }
      );
      $pK = function(dynamic $x) use ($call1,$caml_get_public_method) {
        return $call1($caml_get_public_method($x, 821429468, 266), $x);
      };
      ((function(dynamic $t3, dynamic $t1, dynamic $t2, dynamic $param) {return $t3->observe($t1, $t2);
        })($obs, $node, $cfg, $pK));
      return $obs;
    };
    $Js_of_ocaml_MutationObserver = Vector{
      0,
      $empty_mutation_observer_init,
      $mutationObserver,
      $is_supported,
      $observe
    };
    
    $runtime["caml_register_global"](
      11,
      $Js_of_ocaml_MutationObserver,
      "Js_of_ocaml__MutationObserver"
    );

  }
}
<?hh
// Copyright 2004-present Facebook. All Rights Reserved.

/**
 * Map.php
 */

namespace Rehack;

final class Map {
  <<__Memoize>>
  public static function get() {
    $global_object = \Rehack\GlobalObject::get();
    $runtime = \Rehack\Runtime::get();
    /*
     * Soon, these will replace the `global_data->ModuleName`
     * pattern in the load() function.
     */
    $Pervasives = Pervasives::get();
    $Not_found = Not_found::get();
    $Assert_failure = Assert_failure::get();
    Map::load($global_object);
    $memoized = $runtime->caml_get_global_data()->Map;
    return $memoized;
  }

  /**
   * Performs module load operation. May have side effects.
   */
  private static function load($joo_global_object) {
    

    $runtime = $joo_global_object->jsoo_runtime;
    $caml_arity_test = $runtime["caml_arity_test"];
    $caml_new_string = $runtime["caml_new_string"];
    $call1 = function(dynamic $f, dynamic $a0) use ($caml_arity_test,$runtime) {
      return $caml_arity_test($f) === 1
        ? $f($a0)
        : ($runtime["caml_call_gen"]($f, varray[$a0]));
    };
    $call2 = function(dynamic $f, dynamic $a0, dynamic $a1) use ($caml_arity_test,$runtime) {
      return $caml_arity_test($f) === 2
        ? $f($a0, $a1)
        : ($runtime["caml_call_gen"]($f, varray[$a0,$a1]));
    };
    $call3 = function(dynamic $f, dynamic $a0, dynamic $a1, dynamic $a2) use ($caml_arity_test,$runtime) {
      return $caml_arity_test($f) === 3
        ? $f($a0, $a1, $a2)
        : ($runtime["caml_call_gen"]($f, varray[$a0,$a1,$a2]));
    };
    $global_data = $runtime["caml_get_global_data"]();
    $cst_Map_remove_min_elt = $caml_new_string("Map.remove_min_elt");
    $cst_Map_bal = $caml_new_string("Map.bal");
    $cst_Map_bal__0 = $caml_new_string("Map.bal");
    $cst_Map_bal__1 = $caml_new_string("Map.bal");
    $cst_Map_bal__2 = $caml_new_string("Map.bal");
    $Not_found = $global_data["Not_found"];
    $Pervasives = $global_data["Pervasives"];
    $Assert_failure = $global_data["Assert_failure"];
    $gf = Vector{0, 0, 0, 0};
    $gg = Vector{0, $caml_new_string("map.ml"), 393, 10};
    $gh = Vector{0, 0, 0};
    $Make = function(dynamic $Ord) use ($Assert_failure,$Not_found,$Pervasives,$call1,$call2,$call3,$cst_Map_bal,$cst_Map_bal__0,$cst_Map_bal__1,$cst_Map_bal__2,$cst_Map_remove_min_elt,$gf,$gg,$gh,$runtime) {
      $add = new Ref();
      $add_max_binding = new Ref();
      $add_min_binding = new Ref();
      $bindings_aux = new Ref();
      $cardinal = new Ref();
      $exists = new Ref();
      $filter = new Ref();
      $fold = new Ref();
      $for_all = new Ref();
      $iter = new Ref();
      $join = new Ref();
      $map = new Ref();
      $mapi = new Ref();
      $merge = new Ref();
      $partition = new Ref();
      $remove = new Ref();
      $remove_min_binding = new Ref();
      $split = new Ref();
      $union = new Ref();
      $update = new Ref();
      $height = function(dynamic $param) {
        if ($param) {$h = $param[5];return $h;}
        return 0;
      };
      $create = function(dynamic $l, dynamic $x, dynamic $d, dynamic $r) use ($height) {
        $hl = $height($l);
        $hr = $height($r);
        $gS = $hr <= $hl ? (int) ($hl + 1) : ((int) ($hr + 1));
        return Vector{0, $l, $x, $d, $r, $gS};
      };
      $singleton = function(dynamic $x, dynamic $d) {
        return Vector{0, 0, $x, $d, 0, 1};
      };
      $bal = function(dynamic $l, dynamic $x, dynamic $d, dynamic $r) use ($Pervasives,$call1,$create,$cst_Map_bal,$cst_Map_bal__0,$cst_Map_bal__1,$cst_Map_bal__2,$height) {
        if ($l) {
          $h = $l[5];
          $hl = $h;
        }
        else {$hl = 0;}
        if ($r) {
          $h__0 = $r[5];
          $hr = $h__0;
        }
        else {$hr = 0;}
        if ((int) ($hr + 2) < $hl) {
          if ($l) {
            $lr = $l[4];
            $ld = $l[3];
            $lv = $l[2];
            $ll = $l[1];
            $gN = $height($lr);
            if ($gN <= $height($ll)) {
              return $create($ll, $lv, $ld, $create($lr, $x, $d, $r));
            }
            if ($lr) {
              $lrr = $lr[4];
              $lrd = $lr[3];
              $lrv = $lr[2];
              $lrl = $lr[1];
              $gO = $create($lrr, $x, $d, $r);
              return $create($create($ll, $lv, $ld, $lrl), $lrv, $lrd, $gO);
            }
            return $call1($Pervasives[1], $cst_Map_bal);
          }
          return $call1($Pervasives[1], $cst_Map_bal__0);
        }
        if ((int) ($hl + 2) < $hr) {
          if ($r) {
            $rr = $r[4];
            $rd = $r[3];
            $rv = $r[2];
            $rl = $r[1];
            $gP = $height($rl);
            if ($gP <= $height($rr)) {
              return $create($create($l, $x, $d, $rl), $rv, $rd, $rr);
            }
            if ($rl) {
              $rlr = $rl[4];
              $rld = $rl[3];
              $rlv = $rl[2];
              $rll = $rl[1];
              $gQ = $create($rlr, $rv, $rd, $rr);
              return $create($create($l, $x, $d, $rll), $rlv, $rld, $gQ);
            }
            return $call1($Pervasives[1], $cst_Map_bal__1);
          }
          return $call1($Pervasives[1], $cst_Map_bal__2);
        }
        $gR = $hr <= $hl ? (int) ($hl + 1) : ((int) ($hr + 1));
        return Vector{0, $l, $x, $d, $r, $gR};
      };
      $empty = 0;
      $is_empty = function(dynamic $param) {return $param ? 0 : (1);};
      $add->contents = function(dynamic $x, dynamic $data, dynamic $m) use ($Ord,$add,$bal,$call2) {
        if ($m) {
          $h = $m[5];
          $r = $m[4];
          $d = $m[3];
          $v = $m[2];
          $l = $m[1];
          $c = $call2($Ord[1], $x, $v);
          if (0 === $c) {
            return $d === $data ? $m : (Vector{0, $l, $x, $data, $r, $h});
          }
          if (0 <= $c) {
            $rr = $add->contents($x, $data, $r);
            return $r === $rr ? $m : ($bal($l, $v, $d, $rr));
          }
          $ll = $add->contents($x, $data, $l);
          return $l === $ll ? $m : ($bal($ll, $v, $d, $r));
        }
        return Vector{0, 0, $x, $data, 0, 1};
      };
      $find = function(dynamic $x, dynamic $param) use ($Not_found,$Ord,$call2,$runtime) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            $c = $call2($Ord[1], $x, $v);
            if (0 === $c) {return $d;}
            $param__1 = 0 <= $c ? $r : ($l);
            $param__0 = $param__1;
            continue;
          }
          throw $runtime["caml_wrap_thrown_exception"]($Not_found) as \Throwable;
        }
      };
      $find_first_aux = function
      (dynamic $v0, dynamic $d0, dynamic $f, dynamic $param) use ($call1) {
        $v0__0 = $v0;
        $d0__0 = $d0;
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {
              $v0__0 = $v;
              $d0__0 = $d;
              $param__0 = $l;
              continue;
            }
            $param__0 = $r;
            continue;
          }
          return Vector{0, $v0__0, $d0__0};
        }
      };
      $find_first = function(dynamic $f, dynamic $param) use ($Not_found,$call1,$find_first_aux,$runtime) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {return $find_first_aux($v, $d, $f, $l);}
            $param__0 = $r;
            continue;
          }
          throw $runtime["caml_wrap_thrown_exception"]($Not_found) as \Throwable;
        }
      };
      $find_first_opt_aux = function
      (dynamic $v0, dynamic $d0, dynamic $f, dynamic $param) use ($call1) {
        $v0__0 = $v0;
        $d0__0 = $d0;
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {
              $v0__0 = $v;
              $d0__0 = $d;
              $param__0 = $l;
              continue;
            }
            $param__0 = $r;
            continue;
          }
          return Vector{0, Vector{0, $v0__0, $d0__0}};
        }
      };
      $find_first_opt = function(dynamic $f, dynamic $param) use ($call1,$find_first_opt_aux) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {return $find_first_opt_aux($v, $d, $f, $l);}
            $param__0 = $r;
            continue;
          }
          return 0;
        }
      };
      $find_last_aux = function
      (dynamic $v0, dynamic $d0, dynamic $f, dynamic $param) use ($call1) {
        $v0__0 = $v0;
        $d0__0 = $d0;
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {
              $v0__0 = $v;
              $d0__0 = $d;
              $param__0 = $r;
              continue;
            }
            $param__0 = $l;
            continue;
          }
          return Vector{0, $v0__0, $d0__0};
        }
      };
      $find_last = function(dynamic $f, dynamic $param) use ($Not_found,$call1,$find_last_aux,$runtime) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {return $find_last_aux($v, $d, $f, $r);}
            $param__0 = $l;
            continue;
          }
          throw $runtime["caml_wrap_thrown_exception"]($Not_found) as \Throwable;
        }
      };
      $find_last_opt_aux = function
      (dynamic $v0, dynamic $d0, dynamic $f, dynamic $param) use ($call1) {
        $v0__0 = $v0;
        $d0__0 = $d0;
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {
              $v0__0 = $v;
              $d0__0 = $d;
              $param__0 = $r;
              continue;
            }
            $param__0 = $l;
            continue;
          }
          return Vector{0, Vector{0, $v0__0, $d0__0}};
        }
      };
      $find_last_opt = function(dynamic $f, dynamic $param) use ($call1,$find_last_opt_aux) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            if ($call1($f, $v)) {return $find_last_opt_aux($v, $d, $f, $r);}
            $param__0 = $l;
            continue;
          }
          return 0;
        }
      };
      $find_opt = function(dynamic $x, dynamic $param) use ($Ord,$call2) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            $c = $call2($Ord[1], $x, $v);
            if (0 === $c) {return Vector{0, $d};}
            $param__1 = 0 <= $c ? $r : ($l);
            $param__0 = $param__1;
            continue;
          }
          return 0;
        }
      };
      $mem = function(dynamic $x, dynamic $param) use ($Ord,$call2) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $v = $param__0[2];
            $l = $param__0[1];
            $c = $call2($Ord[1], $x, $v);
            $gM = 0 === $c ? 1 : (0);
            if ($gM) {return $gM;}
            $param__1 = 0 <= $c ? $r : ($l);
            $param__0 = $param__1;
            continue;
          }
          return 0;
        }
      };
      $min_binding = function(dynamic $param) use ($Not_found,$runtime) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $gL = $param__0[1];
            if ($gL) {$param__0 = $gL;continue;}
            $d = $param__0[3];
            $v = $param__0[2];
            return Vector{0, $v, $d};
          }
          throw $runtime["caml_wrap_thrown_exception"]($Not_found) as \Throwable;
        }
      };
      $min_binding_opt = function(dynamic $param) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $gK = $param__0[1];
            if ($gK) {$param__0 = $gK;continue;}
            $d = $param__0[3];
            $v = $param__0[2];
            return Vector{0, Vector{0, $v, $d}};
          }
          return 0;
        }
      };
      $max_binding = function(dynamic $param) use ($Not_found,$runtime) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $gH = $param__0[4];
            $gI = $param__0[3];
            $gJ = $param__0[2];
            if ($gH) {$param__0 = $gH;continue;}
            return Vector{0, $gJ, $gI};
          }
          throw $runtime["caml_wrap_thrown_exception"]($Not_found) as \Throwable;
        }
      };
      $max_binding_opt = function(dynamic $param) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $gE = $param__0[4];
            $gF = $param__0[3];
            $gG = $param__0[2];
            if ($gE) {$param__0 = $gE;continue;}
            return Vector{0, Vector{0, $gG, $gF}};
          }
          return 0;
        }
      };
      $remove_min_binding->contents = function(dynamic $param) use ($Pervasives,$bal,$call1,$cst_Map_remove_min_elt,$remove_min_binding) {
        if ($param) {
          $gD = $param[1];
          if ($gD) {
            $r = $param[4];
            $d = $param[3];
            $v = $param[2];
            return $bal($remove_min_binding->contents($gD), $v, $d, $r);
          }
          $r__0 = $param[4];
          return $r__0;
        }
        return $call1($Pervasives[1], $cst_Map_remove_min_elt);
      };
      $gk = function(dynamic $t, dynamic $match) use ($bal,$min_binding,$remove_min_binding) {
        if ($t) {
          if ($match) {
            $match__0 = $min_binding($match);
            $d = $match__0[2];
            $x = $match__0[1];
            return $bal($t, $x, $d, $remove_min_binding->contents($match));
          }
          return $t;
        }
        return $match;
      };
      $remove->contents = function(dynamic $x, dynamic $m) use ($Ord,$bal,$call2,$gk,$remove) {
        if ($m) {
          $r = $m[4];
          $d = $m[3];
          $v = $m[2];
          $l = $m[1];
          $c = $call2($Ord[1], $x, $v);
          if (0 === $c) {return $gk($l, $r);}
          if (0 <= $c) {
            $rr = $remove->contents($x, $r);
            return $r === $rr ? $m : ($bal($l, $v, $d, $rr));
          }
          $ll = $remove->contents($x, $l);
          return $l === $ll ? $m : ($bal($ll, $v, $d, $r));
        }
        return 0;
      };
      $update->contents = function(dynamic $x, dynamic $f, dynamic $m) use ($Ord,$bal,$call1,$call2,$gk,$update) {
        if ($m) {
          $h = $m[5];
          $r = $m[4];
          $d = $m[3];
          $v = $m[2];
          $l = $m[1];
          $c = $call2($Ord[1], $x, $v);
          if (0 === $c) {
            $match = $call1($f, Vector{0, $d});
            if ($match) {
              $data = $match[1];
              return $d === $data ? $m : (Vector{0, $l, $x, $data, $r, $h});
            }
            return $gk($l, $r);
          }
          if (0 <= $c) {
            $rr = $update->contents($x, $f, $r);
            return $r === $rr ? $m : ($bal($l, $v, $d, $rr));
          }
          $ll = $update->contents($x, $f, $l);
          return $l === $ll ? $m : ($bal($ll, $v, $d, $r));
        }
        $match__0 = $call1($f, 0);
        if ($match__0) {
          $data__0 = $match__0[1];
          return Vector{0, 0, $x, $data__0, 0, 1};
        }
        return 0;
      };
      $iter->contents = function(dynamic $f, dynamic $param) use ($call2,$iter) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $param__1 = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            $iter->contents($f, $l);
            $call2($f, $v, $d);
            $param__0 = $param__1;
            continue;
          }
          return 0;
        }
      };
      $map->contents = function(dynamic $f, dynamic $param) use ($call1,$map) {
        if ($param) {
          $h = $param[5];
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          $l__0 = $map->contents($f, $l);
          $d__0 = $call1($f, $d);
          $r__0 = $map->contents($f, $r);
          return Vector{0, $l__0, $v, $d__0, $r__0, $h};
        }
        return 0;
      };
      $mapi->contents = function(dynamic $f, dynamic $param) use ($call2,$mapi) {
        if ($param) {
          $h = $param[5];
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          $l__0 = $mapi->contents($f, $l);
          $d__0 = $call2($f, $v, $d);
          $r__0 = $mapi->contents($f, $r);
          return Vector{0, $l__0, $v, $d__0, $r__0, $h};
        }
        return 0;
      };
      $fold->contents = function(dynamic $f, dynamic $m, dynamic $accu) use ($call3,$fold) {
        $m__0 = $m;
        $accu__0 = $accu;
        for (;;) {
          if ($m__0) {
            $m__1 = $m__0[4];
            $d = $m__0[3];
            $v = $m__0[2];
            $l = $m__0[1];
            $accu__1 = $call3($f, $v, $d, $fold->contents($f, $l, $accu__0));
            $m__0 = $m__1;
            $accu__0 = $accu__1;
            continue;
          }
          return $accu__0;
        }
      };
      $for_all->contents = function(dynamic $p, dynamic $param) use ($call2,$for_all) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            $gA = $call2($p, $v, $d);
            if ($gA) {
              $gB = $for_all->contents($p, $l);
              if ($gB) {$param__0 = $r;continue;}
              $gC = $gB;
            }
            else {$gC = $gA;}
            return $gC;
          }
          return 1;
        }
      };
      $exists->contents = function(dynamic $p, dynamic $param) use ($call2,$exists) {
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $l = $param__0[1];
            $gx = $call2($p, $v, $d);
            if ($gx) {
              $gy = $gx;
            }
            else {
              $gz = $exists->contents($p, $l);
              if (! $gz) {$param__0 = $r;continue;}
              $gy = $gz;
            }
            return $gy;
          }
          return 0;
        }
      };
      $add_min_binding->contents = function
      (dynamic $k, dynamic $x, dynamic $param) use ($add_min_binding,$bal,$singleton) {
        if ($param) {
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          return $bal($add_min_binding->contents($k, $x, $l), $v, $d, $r);
        }
        return $singleton($k, $x);
      };
      $add_max_binding->contents = function
      (dynamic $k, dynamic $x, dynamic $param) use ($add_max_binding,$bal,$singleton) {
        if ($param) {
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          return $bal($l, $v, $d, $add_max_binding->contents($k, $x, $r));
        }
        return $singleton($k, $x);
      };
      $join->contents = function
      (dynamic $l, dynamic $v, dynamic $d, dynamic $r) use ($add_max_binding,$add_min_binding,$bal,$create,$join) {
        if ($l) {
          if ($r) {
            $rh = $r[5];
            $rr = $r[4];
            $rd = $r[3];
            $rv = $r[2];
            $rl = $r[1];
            $lh = $l[5];
            $lr = $l[4];
            $ld = $l[3];
            $lv = $l[2];
            $ll = $l[1];
            return (int) ($rh + 2) < $lh
              ? $bal($ll, $lv, $ld, $join->contents($lr, $v, $d, $r))
              : ((int) ($lh + 2) < $rh
               ? $bal($join->contents($l, $v, $d, $rl), $rv, $rd, $rr)
               : ($create($l, $v, $d, $r)));
          }
          return $add_max_binding->contents($v, $d, $l);
        }
        return $add_min_binding->contents($v, $d, $r);
      };
      $concat = function(dynamic $t, dynamic $match) use ($join,$min_binding,$remove_min_binding) {
        if ($t) {
          if ($match) {
            $match__0 = $min_binding($match);
            $d = $match__0[2];
            $x = $match__0[1];
            return $join->contents(
              $t,
              $x,
              $d,
              $remove_min_binding->contents($match)
            );
          }
          return $t;
        }
        return $match;
      };
      $concat_or_join = function
      (dynamic $t1, dynamic $v, dynamic $d, dynamic $t2) use ($concat,$join) {
        if ($d) {$d__0 = $d[1];return $join->contents($t1, $v, $d__0, $t2);}
        return $concat($t1, $t2);
      };
      $split->contents = function(dynamic $x, dynamic $param) use ($Ord,$call2,$gf,$join,$split) {
        if ($param) {
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          $c = $call2($Ord[1], $x, $v);
          if (0 === $c) {return Vector{0, $l, Vector{0, $d}, $r};}
          if (0 <= $c) {
            $match = $split->contents($x, $r);
            $rr = $match[3];
            $pres = $match[2];
            $lr = $match[1];
            return Vector{0, $join->contents($l, $v, $d, $lr), $pres, $rr};
          }
          $match__0 = $split->contents($x, $l);
          $rl = $match__0[3];
          $pres__0 = $match__0[2];
          $ll = $match__0[1];
          return Vector{0, $ll, $pres__0, $join->contents($rl, $v, $d, $r)};
        }
        return $gf;
      };
      $merge->contents = function(dynamic $f, dynamic $s1, dynamic $s2) use ($Assert_failure,$call3,$concat_or_join,$gg,$height,$merge,$runtime,$split) {
        if ($s1) {
          $h1 = $s1[5];
          $r1 = $s1[4];
          $d1 = $s1[3];
          $v1 = $s1[2];
          $l1 = $s1[1];
          if ($height($s2) <= $h1) {
            $match = $split->contents($v1, $s2);
            $r2 = $match[3];
            $d2 = $match[2];
            $l2 = $match[1];
            $gt = $merge->contents($f, $r1, $r2);
            $gu = $call3($f, $v1, Vector{0, $d1}, $d2);
            return $concat_or_join(
              $merge->contents($f, $l1, $l2),
              $v1,
              $gu,
              $gt
            );
          }
        }
        else {if (! $s2) {return 0;}}
        if ($s2) {
          $r2__0 = $s2[4];
          $d2__0 = $s2[3];
          $v2 = $s2[2];
          $l2__0 = $s2[1];
          $match__0 = $split->contents($v2, $s1);
          $r1__0 = $match__0[3];
          $d1__0 = $match__0[2];
          $l1__0 = $match__0[1];
          $gv = $merge->contents($f, $r1__0, $r2__0);
          $gw = $call3($f, $v2, $d1__0, Vector{0, $d2__0});
          return $concat_or_join(
            $merge->contents($f, $l1__0, $l2__0),
            $v2,
            $gw,
            $gv
          );
        }
        throw $runtime["caml_wrap_thrown_exception"](Vector{0, $Assert_failure, $gg}) as \Throwable;
      };
      $union->contents = function(dynamic $f, dynamic $s1, dynamic $s2) use ($call3,$concat_or_join,$join,$split,$union) {
        if ($s1) {
          if ($s2) {
            $h2 = $s2[5];
            $r2 = $s2[4];
            $d2 = $s2[3];
            $v2 = $s2[2];
            $l2 = $s2[1];
            $h1 = $s1[5];
            $r1 = $s1[4];
            $d1 = $s1[3];
            $v1 = $s1[2];
            $l1 = $s1[1];
            if ($h2 <= $h1) {
              $match = $split->contents($v1, $s2);
              $r2__0 = $match[3];
              $d2__0 = $match[2];
              $l2__0 = $match[1];
              $l = $union->contents($f, $l1, $l2__0);
              $r = $union->contents($f, $r1, $r2__0);
              if ($d2__0) {
                $d2__1 = $d2__0[1];
                return $concat_or_join(
                  $l,
                  $v1,
                  $call3($f, $v1, $d1, $d2__1),
                  $r
                );
              }
              return $join->contents($l, $v1, $d1, $r);
            }
            $match__0 = $split->contents($v2, $s1);
            $r1__0 = $match__0[3];
            $d1__0 = $match__0[2];
            $l1__0 = $match__0[1];
            $l__0 = $union->contents($f, $l1__0, $l2);
            $r__0 = $union->contents($f, $r1__0, $r2);
            if ($d1__0) {
              $d1__1 = $d1__0[1];
              return $concat_or_join(
                $l__0,
                $v2,
                $call3($f, $v2, $d1__1, $d2),
                $r__0
              );
            }
            return $join->contents($l__0, $v2, $d2, $r__0);
          }
          $s = $s1;
        }
        else {$s = $s2;}
        return $s;
      };
      $filter->contents = function(dynamic $p, dynamic $m) use ($call2,$concat,$filter,$join) {
        if ($m) {
          $r = $m[4];
          $d = $m[3];
          $v = $m[2];
          $l = $m[1];
          $l__0 = $filter->contents($p, $l);
          $pvd = $call2($p, $v, $d);
          $r__0 = $filter->contents($p, $r);
          if ($pvd) {
            if ($l === $l__0) {if ($r === $r__0) {return $m;}}
            return $join->contents($l__0, $v, $d, $r__0);
          }
          return $concat($l__0, $r__0);
        }
        return 0;
      };
      $partition->contents = function(dynamic $p, dynamic $param) use ($call2,$concat,$gh,$join,$partition) {
        if ($param) {
          $r = $param[4];
          $d = $param[3];
          $v = $param[2];
          $l = $param[1];
          $match = $partition->contents($p, $l);
          $lf = $match[2];
          $lt = $match[1];
          $pvd = $call2($p, $v, $d);
          $match__0 = $partition->contents($p, $r);
          $rf = $match__0[2];
          $rt = $match__0[1];
          if ($pvd) {
            $gr = $concat($lf, $rf);
            return Vector{0, $join->contents($lt, $v, $d, $rt), $gr};
          }
          $gs = $join->contents($lf, $v, $d, $rf);
          return Vector{0, $concat($lt, $rt), $gs};
        }
        return $gh;
      };
      $cons_enum = function(dynamic $m, dynamic $e) {
        $m__0 = $m;
        $e__0 = $e;
        for (;;) {
          if ($m__0) {
            $r = $m__0[4];
            $d = $m__0[3];
            $v = $m__0[2];
            $m__1 = $m__0[1];
            $e__1 = Vector{0, $v, $d, $r, $e__0};
            $m__0 = $m__1;
            $e__0 = $e__1;
            continue;
          }
          return $e__0;
        }
      };
      $compare = function(dynamic $cmp, dynamic $m1, dynamic $m2) use ($Ord,$call2,$cons_enum) {
        $compare_aux = function(dynamic $e1, dynamic $e2) use ($Ord,$call2,$cmp,$cons_enum) {
          $e1__0 = $e1;
          $e2__0 = $e2;
          for (;;) {
            if ($e1__0) {
              if ($e2__0) {
                $e2__1 = $e2__0[4];
                $r2 = $e2__0[3];
                $d2 = $e2__0[2];
                $v2 = $e2__0[1];
                $e1__1 = $e1__0[4];
                $r1 = $e1__0[3];
                $d1 = $e1__0[2];
                $v1 = $e1__0[1];
                $c = $call2($Ord[1], $v1, $v2);
                if (0 === $c) {
                  $c__0 = $call2($cmp, $d1, $d2);
                  if (0 === $c__0) {
                    $e2__2 = $cons_enum($r2, $e2__1);
                    $e1__2 = $cons_enum($r1, $e1__1);
                    $e1__0 = $e1__2;
                    $e2__0 = $e2__2;
                    continue;
                  }
                  return $c__0;
                }
                return $c;
              }
              return 1;
            }
            return $e2__0 ? -1 : (0);
          }
        };
        $gq = $cons_enum($m2, 0);
        return $compare_aux($cons_enum($m1, 0), $gq);
      };
      $equal = function(dynamic $cmp, dynamic $m1, dynamic $m2) use ($Ord,$call2,$cons_enum) {
        $equal_aux = function(dynamic $e1, dynamic $e2) use ($Ord,$call2,$cmp,$cons_enum) {
          $e1__0 = $e1;
          $e2__0 = $e2;
          for (;;) {
            if ($e1__0) {
              if ($e2__0) {
                $e2__1 = $e2__0[4];
                $r2 = $e2__0[3];
                $d2 = $e2__0[2];
                $v2 = $e2__0[1];
                $e1__1 = $e1__0[4];
                $r1 = $e1__0[3];
                $d1 = $e1__0[2];
                $v1 = $e1__0[1];
                $gn = 0 === $call2($Ord[1], $v1, $v2) ? 1 : (0);
                if ($gn) {
                  $go = $call2($cmp, $d1, $d2);
                  if ($go) {
                    $e2__2 = $cons_enum($r2, $e2__1);
                    $e1__2 = $cons_enum($r1, $e1__1);
                    $e1__0 = $e1__2;
                    $e2__0 = $e2__2;
                    continue;
                  }
                  $gp = $go;
                }
                else {$gp = $gn;}
                return $gp;
              }
              return 0;
            }
            return $e2__0 ? 0 : (1);
          }
        };
        $gm = $cons_enum($m2, 0);
        return $equal_aux($cons_enum($m1, 0), $gm);
      };
      $cardinal->contents = function(dynamic $param) use ($cardinal) {
        if ($param) {
          $r = $param[4];
          $l = $param[1];
          $gl = $cardinal->contents($r);
          return (int) ((int) ($cardinal->contents($l) + 1) + $gl);
        }
        return 0;
      };
      $bindings_aux->contents = function(dynamic $accu, dynamic $param) use ($bindings_aux) {
        $accu__0 = $accu;
        $param__0 = $param;
        for (;;) {
          if ($param__0) {
            $r = $param__0[4];
            $d = $param__0[3];
            $v = $param__0[2];
            $param__1 = $param__0[1];
            $accu__1 = Vector{
              0,
              Vector{0, $v, $d},
              $bindings_aux->contents($accu__0, $r)
            };
            $accu__0 = $accu__1;
            $param__0 = $param__1;
            continue;
          }
          return $accu__0;
        }
      };
      $bindings = function(dynamic $s) use ($bindings_aux) {
        return $bindings_aux->contents(0, $s);
      };
      return Vector{
        0,
        $height,
        $create,
        $singleton,
        $bal,
        $empty,
        $is_empty,
        $add->contents,
        $find,
        $find_first_aux,
        $find_first,
        $find_first_opt_aux,
        $find_first_opt,
        $find_last_aux,
        $find_last,
        $find_last_opt_aux,
        $find_last_opt,
        $find_opt,
        $mem,
        $min_binding,
        $min_binding_opt,
        $max_binding,
        $max_binding_opt,
        $remove_min_binding->contents,
        $remove->contents,
        $update->contents,
        $iter->contents,
        $map->contents,
        $mapi->contents,
        $fold->contents,
        $for_all->contents,
        $exists->contents,
        $add_min_binding->contents,
        $add_max_binding->contents,
        $join->contents,
        $concat,
        $concat_or_join,
        $split->contents,
        $merge->contents,
        $union->contents,
        $filter->contents,
        $partition->contents,
        $cons_enum,
        $compare,
        $equal,
        $cardinal->contents,
        $bindings_aux->contents,
        $bindings,
        $min_binding,
        $min_binding_opt
      };
    };
    $Map = Vector{
      0,
      function(dynamic $gi) use ($Make) {
        $gj = $Make($gi);
        return Vector{
          0,
          $gj[5],
          $gj[6],
          $gj[18],
          $gj[7],
          $gj[25],
          $gj[3],
          $gj[24],
          $gj[38],
          $gj[39],
          $gj[43],
          $gj[44],
          $gj[26],
          $gj[29],
          $gj[30],
          $gj[31],
          $gj[40],
          $gj[41],
          $gj[45],
          $gj[47],
          $gj[19],
          $gj[20],
          $gj[21],
          $gj[22],
          $gj[48],
          $gj[49],
          $gj[37],
          $gj[8],
          $gj[17],
          $gj[10],
          $gj[12],
          $gj[14],
          $gj[16],
          $gj[27],
          $gj[28]
        };
      }
    };
    
    $runtime["caml_register_global"](11, $Map, "Map");

  }
}

#![feature(globs)]

extern crate dual;
use dual::Dual;

use std::num::*;

fn newton<F:Float>(f: |Dual<F>| -> Dual<F>, s:F, eps:F) -> F {
 let Dual{ val:fx, der:dx } = f(Dual{ val:s, der:Float::one() });
 if fx.abs() < eps {
  s
 } else {
  newton(f, s - fx/dx, eps)
 }
}

fn main() {
 let two:Dual<f64> = Dual{ val:2.0, der:0.0};
 println!("{}", newton(|x:Dual<f64>| { x*x - two}, 2.0, 0.0000000001));
}

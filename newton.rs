
extern crate dual;
extern crate num;
use dual::Dual;

use num::*;

fn newton<F:Float, Fun:Fn(Dual<F>)->Dual<F>>(f:Fun, s:F, eps:F) -> F {
 let Dual{ val:fx, der:dx } = f(Dual{ val:s, der:One::one() });
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


#![feature(globs)]

extern crate dual;

fn main() {
 use std::num::*;
 let z:dual::Dual<f64> = Zero::zero();
 let o:dual::Dual<f64> = One::one();
 let o1:dual::Dual<f64> = match NumCast::from(1u8) { Some(v) => v, None => panic!("from") };
 let x = z + (o+o.clone())*(o1+o)/(o+o1);
 println!("{} {} {}", x.log2().sin().atanh().val, x.der, (-x).abs()!=x);
}

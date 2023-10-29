use std::time::{SystemTime};
use std::thread;

fn quicksort<T: Ord>(arr: &mut Vec<T>) {
    let len = arr.len();
    if len <= 1 {return ();}

    fn qs<T: Ord>(arr: &mut Vec<T>, start: usize, end: usize) {
        if start >= end {return;}
        
        let mut pivot_target: usize = start;

        for focus in start+1..=end {
            if arr[start] > arr[focus] {
                pivot_target += 1;
                arr.swap(focus, pivot_target);
            }
        }

        arr.swap(start, pivot_target);

        qs(arr, start, if pivot_target == 0 {0} else {pivot_target - 1});
        qs(arr, pivot_target + 1, end);
    }

    qs(arr, 0, len - 1);
}

fn is_sorted<T: Ord>(arr: &mut Vec<T>) -> bool {
    let len = arr.len();
    if len <= 1 {return true;}
    for ind in 0..len-1 {
        if arr[ind] > arr[ind+1] {return false;};
    }

    return true;
}

fn make_list() -> Vec<Vec<i64>> {
    let mut v: Vec<Vec<i64>> = Vec::new();

    for count_outer in 0..10000 {
        let mut inner: Vec<i64> = Vec::new();

        for count_inner in 0..1000 {
            inner.push(rand::random::<i64>());
        }
        v.push(inner);
    }

    return v;
}

const STACK_SIZE: usize = 400 * 1024 * 1024;

fn main() {
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(prog)
        .unwrap();
    child.join().unwrap()
}

fn prog() {
    let v: Vec<Vec<i64>> = make_list();
    let k = SystemTime::now();
    for mut elem in v {
        quicksort(&mut elem);
        if !is_sorted(&mut elem) {println!("False"); break}
    }
    match k.elapsed() {
        Ok(elapsed) => {println!("{}", elapsed.as_nanos())}
        Err(e) => {println!("Error: {e:?}")}
    }
}


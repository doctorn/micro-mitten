#![crate_type = "cdylib"]

use libc::c_void;
use std::cell::RefCell;
use std::collections::HashSet;

thread_local! {
    static ANTI_MARKS: RefCell<HashSet<*mut c_void>> = RefCell::new(HashSet::new());
}

#[no_mangle]
pub extern "C" fn mitten_free(ptr: *mut c_void) {
    ANTI_MARKS.with(|marks| marks.borrow_mut().insert(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn mitten_reset() {
    ANTI_MARKS.with(|marks| {
        for ptr in marks.borrow_mut().drain() {
            libc::free(ptr);
        }
    })
}

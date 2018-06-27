all:
	cargo +nightly build --release --target wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/release/j8tw.wasm --out-dir playground/src


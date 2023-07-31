default:
	cargo run -q -- 1:00+2:00

release:
	cargo build -r

install:
	cargo install --path .

watch:
	cargo watch -c -x clippy

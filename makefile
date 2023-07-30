default:
	cargo run -q -- 8:00+5:00

release:
	cargo build -r

install:
	cargo install --path .

watch:
	cargo watch -c -x clippy

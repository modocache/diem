//# publish
address 0x2 {
    module Coin {
        struct Coin {
            value: u64,
        }
    }
}

//# run
script {
    use 0x2::Coin::Coin;

    fun main() {
        let _coin = Coin { value: 100 };
    }
}

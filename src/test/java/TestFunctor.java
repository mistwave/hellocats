import java.util.Optional;

public class TestFunctor {
    public static void main(String[] args) {
        var t = Optional.empty().map(n -> "hi, " + n);

    }
}

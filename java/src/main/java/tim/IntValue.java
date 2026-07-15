package tim;

public record IntValue(int value) implements Value {
    static Value of(int value) {
        return new IntValue(value);
    }

}

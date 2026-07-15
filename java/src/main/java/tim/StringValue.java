package tim;

public record StringValue(String value) implements Value {
    static Value of(String value) {
        return new StringValue(value);
    }
}
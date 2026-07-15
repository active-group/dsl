package tim;

public interface Value {
    default int getInt() {
        return switch (this) {
            case IntValue(int value) -> value;
            default -> throw new RuntimeException("should be int, but isn't: " + this);
        };
    }

    default String getString() {
        return switch (this) {
            case StringValue(String value) -> value;
            default -> throw new RuntimeException("should be String, but isn't: " + this);
        };
    }
}
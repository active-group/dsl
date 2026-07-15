package tim;

public record Direction(int right, int down) {
    public static Direction of(int right, int down) {
        return new Direction(right, down);
    }
}

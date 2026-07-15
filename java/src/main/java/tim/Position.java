package tim;

public record Position(int x, int y) {
    public static Position of(int x, int y) {
        return new Position(x, y);
    }
    public Position move(Direction direction) {
        return new Position(x + direction.right(), y + direction.down());
    }
}

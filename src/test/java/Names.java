import java.util.ArrayList;
import java.util.List;

public class Names {
    public static void main(String[] args) {

        String[] names = {"a","bob","mike","i"};
        List<String> result = new ArrayList<>();
        for (String name : names) {
            if (name.length() > 1) {
                String newName =
                        name.substring(0, 1).toUpperCase()
                                + name.substring(1);
                result.add(newName);
            }
        }
        System.out.println(result);
        // [Bob, Mike]


    }
}

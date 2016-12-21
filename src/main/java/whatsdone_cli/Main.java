
package whatsdone_cli;

public class Main {

    public static void main(String[] args) {
        System.out.println("App started...");
        DoneRepo repo = new DoneRepo();
        try {
            repo.printTableContents();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}

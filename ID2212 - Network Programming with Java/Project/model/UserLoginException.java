package se.kth.id2212.ex4.shop.model;

/**
 * Thrown when username or password is wrong.
 */
public class UserLoginException extends Exception {

    public UserLoginException(String msg) {
        super(msg);
    }

}

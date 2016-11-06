package se.kth.id2212.ex4.shop.model;

/**
 * Thrown when user already exist.
 */
public class UserNotFoundException extends Exception {

    public UserNotFoundException(String msg) {
        super(msg);
    }

}

package se.kth.id2212.ex4.shop.model;

/**
 * Thrown when user already exist.
 */
public class UserExistException extends Exception {

    public UserExistException(String msg) {
        super(msg);
    }

}

package se.kth.ict.IV1201.project.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import javax.validation.Payload;

/**
 * The annotated target is checked to be a username. A valid username should be
 * at least 6 characters and does not contain any special characters.
 *
 * @author Konstantin Sozinov
 */
@Constraint(validatedBy = ValidUsername.UsernameValidator.class)
@Documented
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidUsername {

    String message() default "{invalidUsername}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    class UsernameValidator implements ConstraintValidator<ValidUsername, String> {

        @Override   
        public void initialize(ValidUsername constraintAnnotation) {
        }

        @Override
        public boolean isValid(String value, ConstraintValidatorContext context) {
            if (isEmpty(value, context)) {
                return false;
            }
           if(!(value.matches("^[a-zA-Z0-9]{6,}$"))) return false;
           return true;
           
        }

        private boolean isEmpty(String value, ConstraintValidatorContext context) {
            if (value.length() == 0) {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate("{emptyUsername}").addConstraintViolation();
                return true;
            }
            return false;
        }
    }
}

package se.kth.ict.IV1201.project.validation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Date;
import javax.validation.Constraint;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import javax.validation.Payload;

/**
 * The annotated target is checked to be a username and password.
 * A valid username and password should be at least 6 characters and
 * does not contain any special characters.
 * 
 * @author Konstantin Sozinov
 */
@Constraint(validatedBy = ValidDate.DateValidator.class)
@Documented
@Target({ElementType.METHOD, ElementType.FIELD, ElementType.ANNOTATION_TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidDate {

    String message() default "{emptyDate}";

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    class DateValidator implements ConstraintValidator<ValidDate, Date> {

        @Override   
        public void initialize(ValidDate constraintAnnotation) {}

        @Override
        public boolean isValid(Date value, ConstraintValidatorContext context) {
            if (value == null)
                return false;
            return true;
        }
    }
}
